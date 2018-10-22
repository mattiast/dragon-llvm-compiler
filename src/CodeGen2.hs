module CodeGen2 where
import StaticAnalysis
import AbstractSyntax
import Control.Monad.State
import Data.Tree
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
import LLVM.IRBuilder.Monad(MonadIRBuilder)
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

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




expr :: (MonadIRBuilder m) => ExprAnn Type -> m L.Operand
expr e =
  case e of
    ENum TInt x -> int32 x
    EReal TFloat x -> double x
    EBool TBool b ->
      bit
        (if b
           then 1
           else 0)
    EBin t "+" e1 e2 -> do
      x1 <- expr e1
      x2 <- expr e2
      let op =
            case t of
              TInt -> add
              TFloat -> fadd
      op x1 x2
