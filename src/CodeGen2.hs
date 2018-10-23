{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module CodeGen2 where
import StaticAnalysis
import AbstractSyntax
import Control.Monad.State
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Map as M

import qualified LLVM.AST as L
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
      True <- pure $ getTag e1 == getTag e2
      let Just op = M.lookup (bop, getTag e1) bopTable
      op x1 x2
    EUn t uop e1 -> do
      x1 <- expr f e1
      let Just op = M.lookup (uop, getTag e1) uopTable
      op x1
    (EArrayInd _ _ _) -> do
      let (EFetch _ v, inds) = extractIndices e
      let Just xv = frameLookup f v
      xinds <- T.traverse (expr f) inds
      zero <- int32 0
      reg_ptr <- gep xv (zero:xinds)
      load reg_ptr 4
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

push_label :: (MonadState [a] m) => a -> m ()
push_label name = modify (name:)

pop_label :: (MonadState [a] m) => m ()
pop_label = modify tail

stmt :: (MonadIRBuilder m, MonadState [L.Name] m) => StmtA (Frame L.Operand) TType -> m ()
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
        push_label lbl_end
        stmt s1
        pop_label
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
        push_label lbl_end
        stmt s1
        pop_label
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
stmt SBreak = do
    break_label <- head <$> get
    br break_label
    lbl_dead <- freshName "dead"
    emitBlockStart lbl_dead
