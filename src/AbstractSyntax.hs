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

data Expr
  = ENum Integer
  | EReal Double
  | EBool Bool
  | EFetch LValue
  | EBin BinOp
         Expr
         Expr
  | EUn UnOp
        Expr
  deriving (Eq, Ord)

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
  show (ENum i) = show i
  show (EReal r) = show r
  show (EBool b) = show b
  show (EFetch lv) = show lv
  show (EBin bop e1 e2) = "(" ++ show e1 ++ ")" ++ bop ++ "(" ++ show e2 ++ ")"
  show (EUn uop e1) = uop ++ show e1

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
