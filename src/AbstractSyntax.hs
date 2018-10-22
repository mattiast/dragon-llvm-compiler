{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

{- Grammar:
P  ->  { DD SS }
DD ->  e | DD D | D
D  ->  T id ;
T  ->  T [ num ] | int | float | char | bool
SS ->  e | SS S | S
S  ->  L = E ; | if ( B ) S | if ( B ) S else S | while ( B ) S
   |   do S while ( B ) ; | break ; | { DD SS }
B  ->  B or B | B and B | ! B | ( B ) | E rel E | true | false
E  ->  E + E | E - E | E * E | E / E | L | ( B ) | num
L  ->  L [ B ] | id
-}
module AbstractSyntax where
import GHC.Generics
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Bifunctor.TH

type Var = String

data LValueAnn t
  = LVar Var
  | LArr (LValueAnn t)
         (ExprAnn t)
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

type LValue = LValueAnn ()

data TType
  = TInt
  | TFloat
  | TChar
  | TBool
  | TArr TType
         Int
  deriving (Eq, Ord, Generic)

type BinOp = String

type UnOp = String

type Expr = ExprAnn ()

data ExprAnn t
  = ENum t Integer
  | EReal t Double
  | EBool t Bool
  | EFetch t Var
  | EArrayInd t (ExprAnn t) (ExprAnn t)
  | EBin t BinOp
         (ExprAnn t)
         (ExprAnn t)
  | EUn t UnOp
        (ExprAnn t)
    deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

getTag :: ExprAnn t -> t
getTag (ENum t _) = t
getTag (EReal t _) = t
getTag (EBool t _) = t
getTag (EFetch t _) = t
getTag (EArrayInd t _ _) = t
getTag (EBin t _ _ _) = t
getTag (EUn t _ _) = t

data Decl =
  Decl TType
       Var
  deriving (Eq, Ord, Show)

data StmtA s e
  = SAssign s (LValueAnn e)
            (ExprAnn e)
  | SBlock s [Decl]
           [StmtA s e]
  | SIf s (ExprAnn e)
        (StmtA s e)
        (StmtA s e)
  | SWhile s (ExprAnn e)
           (StmtA s e)
  | SDoWhile s (ExprAnn e)
             (StmtA s e)
  | SBreak
  deriving (Eq, Ord, Show, Generic)


type Stmt = StmtA () ()

-- TODO pretty printing
instance Show (ExprAnn t) where
  show (ENum _ i) = show i
  show (EReal _ r) = show r
  show (EBool _ b) = show b
  show (EFetch _ v) = show v
  show (EArrayInd _ lv i) = show lv ++ "[" ++ show i ++ "]"
  show (EBin _ bop e1 e2) = "(" ++ show e1 ++ ")" ++ bop ++ "(" ++ show e2 ++ ")"
  show (EUn _ uop e1) = uop ++ show e1

instance Show TType where
  show TInt = "int"
  show TFloat = "float"
  show TChar = "char"
  show TBool = "bool"
  show (TArr t i) = show t ++ "[" ++ show i ++ "]"

instance Show (LValueAnn t) where
  show (LVar var) = var
  show (LArr l e) = show l ++ "[" ++ show e ++ "]"

extractLValue :: LValueAnn t -> (Var, [ExprAnn t])
extractLValue (LVar v) = (v, [])
extractLValue (LArr lv e1) =
  let (v, inds) = extractLValue lv
  in (v, inds ++ [e1])

lval2expr :: LValue -> Expr
lval2expr (LVar v) = EFetch () v
lval2expr (LArr lv e) = EArrayInd () (lval2expr lv) e

expr2lval :: ExprAnn t -> Maybe LValue
expr2lval (EFetch _ v) = pure (LVar v)
expr2lval (EArrayInd _ x y) = do
    lx <- expr2lval x
    pure $ LArr lx (fmap (const ()) y)
expr2lval _ = Nothing

$(deriveBifunctor ''StmtA)
$(deriveBifoldable ''StmtA)
$(deriveBitraversable ''StmtA)
