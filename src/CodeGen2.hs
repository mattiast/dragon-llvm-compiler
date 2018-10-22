{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module CodeGen2 where
import StaticAnalysis
import AbstractSyntax
import Control.Monad.State
import Data.Function((&))
import qualified Data.Text.Lazy as T
import Data.String(fromString)
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Map as M

import qualified LLVM.AST as L
import LLVM.Pretty
import LLVM.AST.Type(ptr)
import LLVM.AST.Constant
import LLVM.IRBuilder.Monad(MonadIRBuilder, freshName, emitBlockStart)
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.AST.FloatingPointPredicate hiding (True, False)
import qualified LLVM.AST.IntegerPredicate as I
import Prelude hiding (and, or)

convertType :: TType -> L.Type
convertType tp = case tp of
    TInt -> L.IntegerType 32
    TChar -> L.IntegerType 8
    TBool -> L.IntegerType 1
    TFloat -> L.FloatingPointType L.DoubleFP
    TArr t n -> L.ArrayType (fromIntegral n) (convertType t)

allocations :: ATree (TType,Var) -> [L.Named L.Instruction]
allocations t = do
    (_, x) <- F.toList t
    (tp, var) <- F.toList $ head $ symTables x
    return $ (changeVar var) L.:= L.Alloca (convertType tp) Nothing 4 []

changeVar :: String -> L.Name
changeVar ('%':var) = fromString var

likePtr :: String -> TType -> String -> [String] -> L.Named L.Instruction
likePtr vptr typ var vinds =
  changeVar vptr L.:=
  L.GetElementPtr
    False
    (L.LocalReference (ptr $ convertType typ) (changeVar var))
    (i32 0 : map (ind . changeVar) vinds)
    []

i32 :: Integer -> L.Operand
i32 x = L.ConstantOperand (Int 32 x)

ind :: L.Name -> L.Operand
ind v = L.LocalReference (L.IntegerType 32) v

newEvalPtr :: String -> TType -> String -> [String] -> String
newEvalPtr vptr typ var vinds = likePtr vptr typ var vinds & ppll & T.unpack

likeLoad :: String -> TType -> String -> L.Named L.Instruction
likeLoad v1 typ vptr = (changeVar v1) L.:= L.Load False (L.LocalReference (ptr $ convertType typ) (changeVar vptr)) Nothing 4 []

newLoad :: String -> TType -> String -> String
newLoad v1 typ vptr = likeLoad v1 typ vptr & ppll & T.unpack

bopTable ::
     (MonadIRBuilder m)
  => M.Map (BinOp, TType) (L.Operand -> L.Operand -> m L.Operand)
bopTable = M.fromList
    [ (("+", TInt), add)
    , (("+", TFloat), fadd)
    , (("-", TInt), sub)
    , (("-", TFloat), fsub)
    , (("*", TInt), mul)
    , (("*", TFloat), fmul)
    , (("/", TInt), sdiv)
    , (("/", TFloat), fdiv)
    , (("&&", TBool ), and)
    , (("||", TBool ), or)
    , (("==", TFloat), fcmp OEQ)
    , (("!=", TFloat), fcmp ONE)
    , (("<" , TFloat), fcmp OLT)
    , (("<=", TFloat), fcmp OLE)
    , ((">" , TFloat), fcmp OGT)
    , ((">=", TFloat), fcmp OGE)
    , (("==", TInt), icmp I.EQ)
    , (("!=", TInt), icmp I.NE)
    , (("<" , TInt), icmp I.SLT)
    , (("<=", TInt), icmp I.SLE)
    , ((">" , TInt), icmp I.SGT)
    , ((">=", TInt), icmp I.SGE)
    , (("==", TChar), icmp I.EQ)
    , (("!=", TChar), icmp I.NE)
    , (("<" , TChar), icmp I.SLT)
    , (("<=", TChar), icmp I.SLE)
    , ((">" , TChar), icmp I.SGT)
    , ((">=", TChar), icmp I.SGE)
    , (("==", TBool), icmp I.EQ)
    , (("!=", TBool), icmp I.NE)
    , (("<" , TBool), icmp I.SLT)
    , (("<=", TBool), icmp I.SLE)
    , ((">" , TBool), icmp I.SGT)
    , ((">=", TBool), icmp I.SGE)
    ]

uopTable ::
     (MonadIRBuilder m)
  => M.Map (UnOp, TType) (L.Operand -> m L.Operand)
uopTable = M.fromList
    [ (("-", TInt), int32 0 >>= sub)
    , (("-", TFloat), double 0 >>= fsub)
    , (("!", TBool), bit 1 >>= xor)
    , (("f2i", TFloat), flip fptosi (convertType TInt))
    , (("i2f", TInt), flip sitofp (convertType TFloat))
    ]

expr :: (MonadIRBuilder m) => Frame L.Operand -> ExprAnn TType -> m L.Operand
expr f e =
  case e of
    ENum TInt x -> int32 x
    EReal TFloat x -> double x
    EBool TBool b ->
      bit
        (if b
           then 1
           else 0)
    EBin t bop e1 e2 -> do
      x1 <- expr f e1
      x2 <- expr f e2
      let Just op = M.lookup (bop, t) bopTable
      op x1 x2
    EUn t uop e1 -> do
      x1 <- expr f e1
      let Just op = M.lookup (uop, getTag e1) uopTable
      op x1
    (EArrayInd _ _ _) -> do
      let (v, inds) = extractIndices e
      xv <- expr f v
      xinds <- T.traverse (expr f) inds
      zero <- int32 0
      gep xv (zero:xinds)
    (EFetch _ v) -> do
      let Just xv = frameLookup f v
      load xv 4

extractIndices :: ExprAnn t -> (ExprAnn t, [ExprAnn t])
extractIndices (EArrayInd _ x y) =
    let (f, r) = extractIndices x
    in (f, r ++ [y])
extractIndices e = (e, [])

newnewvars :: (MonadIRBuilder m) => StmtA (Frame TType) t -> m (StmtA (Frame L.Operand) t)
newnewvars = go (Frame []) where
    go f s = case s of
        SBlock (Frame (d:_)) dd ss -> do
            d' <- traverse (\t -> alloca (convertType t) Nothing 4) d
            let f' = Frame (d':symTables f)
            (SBlock f' dd) <$> (traverse (go f') ss)
        SIf _ b s1 s2 -> (SIf f b) <$> (go f s1) <*> (go f s2)
        SDoWhile _ b s1 -> (SDoWhile f b) <$> (go f s1)
        SWhile _ b s1 -> (SWhile f b) <$> (go f s1)
        SAssign _ lv e -> pure (SAssign f lv e)
        SBreak -> pure SBreak

stmt :: (MonadIRBuilder m) => StmtA (Frame L.Operand) TType -> m ()
stmt (SIf f cond tstmt fstmt) = do
    lbl_true <- freshName "if_true"
    lbl_false <- freshName "if_false"
    lbl_end <- freshName "end_if"
    test_reg <- expr f cond
    condBr test_reg lbl_true lbl_false
    emitBlockStart lbl_true
    stmt tstmt
    br lbl_end
    emitBlockStart lbl_false
    stmt fstmt
    br lbl_end
    emitBlockStart lbl_end
stmt (SWhile f b s1) = do
        lbl_test <- freshName "test"
        lbl_begin <- freshName "begin_while"
        lbl_end <- freshName "end_while"
        br lbl_test
        emitBlockStart lbl_begin
        -- push_while
        stmt s1
        -- pop_while
        br lbl_test
        emitBlockStart lbl_test
        test_reg <- expr f b
        condBr test_reg lbl_begin lbl_end
        emitBlockStart lbl_end
stmt (SDoWhile f b s1) = do
        lbl_test <- freshName "test"
        lbl_begin <- freshName "begin_while"
        lbl_end <- freshName "end_while"
        br lbl_begin
        emitBlockStart lbl_begin
        -- push_while
        stmt s1
        -- pop_while
        br lbl_test
        emitBlockStart lbl_test
        test_reg <- expr f b
        condBr test_reg lbl_begin lbl_end
        emitBlockStart lbl_end
stmt (SAssign f lval e) = do
    xe <- expr f e
    let Just reg_var = frameLookup f v
        (v, inds) = extractLValue lval
    xinds <- traverse (expr f) inds
    zero <- int32 0
    reg_ptr <- gep reg_var (zero : xinds)
    store reg_ptr 4 xe
stmt (SBlock _ _ ss) = F.traverse_ stmt ss
stmt SBreak = return ()
