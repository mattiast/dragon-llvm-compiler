{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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

type Var = String

data LValue
  = LVar Var
  | LArr LValue
         Expr
  deriving (Eq, Ord)

data Type
  = TInt
  | TFloat
  | TChar
  | TBool
  | TArr Type
         Int
  deriving (Eq, Ord)

type BinOp = String

type UnOp = String

type Expr = ExprAnn ()

data ExprAnn t
  = ENum t Integer
  | EReal t Double
  | EBool t Bool
  | EFetch t LValue
  | EBin t BinOp
         (ExprAnn t)
         (ExprAnn t)
  | EUn t UnOp
        (ExprAnn t)
    deriving (Eq, Ord)

getTag :: ExprAnn t -> t
getTag (ENum t _) = t

data Decl =
  Decl Type
       Var
  deriving (Eq, Ord, Show)

data Stmt
  = SAssign LValue
            Expr
  | SBlock [Decl]
           [Stmt]
  | SIf Expr
        Stmt
        Stmt
  | SWhile Expr
           Stmt
  | SDoWhile Expr
             Stmt
  | SBreak
  deriving (Eq, Ord, Show)

-- TODO pretty printing
instance Show Expr where
  show (ENum _ i) = show i
  show (EReal _ r) = show r
  show (EBool _ b) = show b
  show (EFetch _ lv) = show lv
  show (EBin _ bop e1 e2) = "(" ++ show e1 ++ ")" ++ bop ++ "(" ++ show e2 ++ ")"
  show (EUn _ uop e1) = uop ++ show e1

instance Show Type where
  show TInt = "int"
  show TFloat = "float"
  show TChar = "char"
  show TBool = "bool"
  show (TArr t i) = show t ++ "[" ++ show i ++ "]"

instance Show LValue where
  show (LVar var) = var
  show (LArr l e) = show l ++ "[" ++ show e ++ "]"

extractLValue :: LValue -> (Var, [Expr])
extractLValue (LVar v) = (v, [])
extractLValue (LArr lv e1) =
  let (v, inds) = extractLValue lv
  in (v, inds ++ [e1])
