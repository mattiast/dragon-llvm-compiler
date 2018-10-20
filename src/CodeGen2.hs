module CodeGen2 where
import StaticAnalysis
import AbstractSyntax
import Control.Monad.State
import Data.Tree
import Data.String(fromString)
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Map as M

import qualified LLVM.AST as L
import LLVM.Pretty

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
    return $ (fromString $ changeVar var) L.:= L.Alloca (convertType tp) Nothing 4 []

changeVar :: String -> String
changeVar ('%':var) = var
