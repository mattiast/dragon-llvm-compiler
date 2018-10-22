module Parser where
import AbstractSyntax
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Data.List
import Control.Applicative


-- ohjelma on pelkkä block-statement
wParse :: String -> Stmt
wParse src = case parse parseStmt "" src of
        Right s -> s
        Left e -> error $ show e

-----------------------
-- Parser
-----------------------

parseLValue :: Parser LValue
parseLValue = do
                id <- identifier
                indices <- many (brackets parseExpr)
                return $ foldl' LArr (LVar id) indices
              <?> "L-value"

parseDeclaration :: Parser Decl
parseDeclaration = do
  t <- parseType
  id <- identifier
  semi
  return $ Decl t id
  <?> "declaration"

parseType :: Parser Type
parseType = do
  prim <- foldr1 (<|>) $ map (\s -> do{reserved s;return s}) ["int", "float", "char", "bool"]
  dims <- many (brackets integer)
  return $ foldl' (\typ dim -> TArr typ (fromInteger dim)) (pr prim) dims
  <?> "type" where
    pr "int" = TInt
    pr "float" = TFloat
    pr "char" = TChar
    pr "bool" = TBool

parseExpr    :: Parser Expr
parseExpr    = buildExpressionParser table factor <?> "expression" 
                where
    table   = [
               [uop "!", uop "-"], -- TODO ongelma: noita voi olla vain yksi, eli !!(x < y), --y ei kelpaa, 
               [bop "*" AssocLeft, bop "/" AssocLeft],
               [bop "+" AssocLeft, bop "-" AssocLeft],
               map (\s -> bop s AssocNone) ["<=",">=","<",">"],
               map (\s -> bop s AssocNone) ["==","!="],
               [bop "&&" AssocLeft],
               [bop "||" AssocLeft]
          ]
        where
          bop s assoc
             = Infix (do{ reservedOp s; return (EBin () s)}) assoc
          uop s
             = Prefix (do{ reservedOp s; return (EUn () s)})

    factor  = try (parens parseExpr)
        <|> try real
        <|> try number
        <|> try truelit
        <|> try falselit
        <|> try lvalue 
        <?> "simple expression"

    truelit = reserved "true" *> pure (EBool () True)
    falselit = reserved "false" *> pure (EBool () False)
    number  = ENum () <$> integer
        <?> "number"
    real  = EReal () <$> float
        <?> "real number"
    lvalue = do
        name <- EFetch () <$> identifier
        indices <- many (brackets parseExpr)
        return $ foldl (EArrayInd ()) name indices
        <?> "variable name"

parseStmt :: Parser Stmt
parseStmt = let
    assign = do
        v <- parseLValue
        reservedOp "="
        a <- parseExpr
        semi
        return $ SAssign () v a
        <?> "assignment statement"
    while = do
        reserved "while"
        b <- parens parseExpr
        s <- stat
        return $ SWhile () b s
        <?> "while statement"
    ifelse_stmt = do
        reserved "if"
        b <- parens parseExpr
        s1 <- stat
        reserved "else"
        s2 <- stat
        return $ SIf () b s1 s2
        <?> "if statement"
    if_stmt = do
        reserved "if"
        b <- parens parseExpr
        s <- stat
        return $ SIf () b s (SBlock () [] [])
        <?> "if statement"
    block = braces (do
        dd <- many parseDeclaration
        ss <- many stat
        return $ SBlock () dd ss)
        <?> "block"
    break = do
        reserved "break"
        semi
        return SBreak
        <?> "break"
    dowhile = do
        reserved "do"
        s <- stat
        reserved "while"
        b <- parens parseExpr
        semi
        return $ SDoWhile () b s
        <?> "do-while statement"
    stat = try assign <|> try while <|> try ifelse_stmt <|> try if_stmt <|> try block <|> try break <|> try dowhile

    in stat <?> "statement"

-----------------------------------------------------------
-- The lexer
-----------------------------------------------------------
lexer     = P.makeTokenParser whileDef

whileDef  = javaStyle
          { 
            P.reservedNames  = [ "true", "false", "do", "while", "if", "else", "break", "int", "float", "char", "bool" ]
          , P.reservedOpNames= ["||", "&&",  "!", "==", "!=", "<=", ">=", "<", ">", "+", "-", "*", "/", "="]
          , P.opLetter       = oneOf (concat (P.reservedOpNames whileDef))
          , P.caseSensitive  = False
          }

parens          = P.parens lexer    
braces          = P.braces lexer    
brackets        = P.brackets lexer
identifier      = P.identifier lexer    
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
float           = P.float lexer
semi            = P.semi lexer
