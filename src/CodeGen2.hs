{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module CodeGen2 where
import StaticAnalysis
import AbstractSyntax
import Control.Monad.State
import Control.Monad.Reader
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

convertType :: Type -> L.Type
convertType tp = case tp of
    TInt -> L.IntegerType 32
    TChar -> L.IntegerType 8
    TBool -> L.IntegerType 1
    TFloat -> L.FloatingPointType L.FloatFP
    TArr t n -> L.ArrayType (fromIntegral n) (convertType t)

allocations :: ATree (Type,Var) -> [L.Named L.Instruction]
allocations t = do
    (_, x) <- F.toList t
    (tp, var) <- F.toList $ symTable x
    return $ (changeVar var) L.:= L.Alloca (convertType tp) Nothing 4 []

changeVar :: String -> L.Name
changeVar ('%':var) = fromString var

likePtr :: String -> Type -> String -> [String] -> L.Named L.Instruction
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

newEvalPtr :: String -> Type -> String -> [String] -> String
newEvalPtr vptr typ var vinds = likePtr vptr typ var vinds & ppll & T.unpack

likeLoad :: String -> Type -> String -> L.Named L.Instruction
likeLoad v1 typ vptr = (changeVar v1) L.:= L.Load False (L.LocalReference (ptr $ convertType typ) (changeVar vptr)) Nothing 4 []

newLoad :: String -> Type -> String -> String
newLoad v1 typ vptr = likeLoad v1 typ vptr & ppll & T.unpack

bopTable ::
     (MonadIRBuilder m)
  => M.Map (BinOp, Type) (L.Operand -> L.Operand -> m L.Operand)
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
  => M.Map (UnOp, Type) (L.Operand -> m L.Operand)
uopTable = M.fromList
    [ (("-", TInt), int32 0 >>= sub)
    , (("-", TFloat), double 0 >>= fsub)
    , (("!", TBool), bit 1 >>= xor)
    , (("f2i", TFloat), flip fptosi (convertType TInt))
    , (("i2f", TInt), flip sitofp (convertType TFloat))
    ]

expr :: (MonadIRBuilder m, MonadReader (M.Map Var L.Operand) m) => ExprAnn Type -> m L.Operand
expr e =
  case e of
    ENum TInt x -> int32 x
    EReal TFloat x -> double x
    EBool TBool b ->
      bit
        (if b
           then 1
           else 0)
    EBin t bop e1 e2 -> do
      x1 <- expr e1
      x2 <- expr e2
      let Just op = M.lookup (bop, t) bopTable
      op x1 x2
    EUn t uop e1 -> do
      x1 <- expr e1
      let Just op = M.lookup (uop, t) uopTable
      op x1
    (EArrayInd _ _ _) -> do
      let (v, inds) = extractIndices e
      xv <- expr v
      xinds <- T.traverse expr inds
      zero <- int32 0
      gep xv (zero:xinds)
    (EFetch _ v) -> do
      table <- ask
      let Just xv = M.lookup v table
      return xv

extractIndices :: ExprAnn t -> (ExprAnn t, [ExprAnn t])
extractIndices (EArrayInd _ x y) =
    let (f, r) = extractIndices x
    in (f, r ++ [y])
extractIndices e = (e, [])

traverseATree :: Applicative f => (a -> f b) -> ATree a -> f (ATree b)
traverseATree h = traverse . traverse . traverse $ h

newallocations :: MonadIRBuilder m => ATree Type -> m (ATree (Type, L.Operand))
newallocations = traverseATree $ \tp -> do
    reg <- alloca (convertType tp) Nothing 4
    return (tp, reg)

stmt :: (MonadIRBuilder m, MonadReader (M.Map Var L.Operand) m) => StmtA Type -> m ()
stmt (SIf cond tstmt fstmt) = do
    lbl_true <- freshName "if_true"
    lbl_false <- freshName "if_false"
    lbl_end <- freshName "end_if"
    test_reg <- expr cond
    condBr test_reg lbl_true lbl_false
    emitBlockStart lbl_true
    stmt tstmt
    br lbl_end
    emitBlockStart lbl_false
    stmt fstmt
    br lbl_end
    emitBlockStart lbl_end
