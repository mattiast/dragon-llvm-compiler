{-# LANGUAGE DeriveFunctor #-}
module StaticAnalysis where
import AbstractSyntax
import Data.Tree
import qualified Data.Map as M
import qualified Data.Foldable as F
import Control.Arrow
import Control.Applicative
-- this is only used to get orphan instance for Alternative (Either String) blehhhh
import Control.Monad.Error
import Data.Function((&))
import Data.Monoid

newtype Frame t = Frame { symTables :: [M.Map String t]} deriving (Eq,Ord,Show, Functor) 

type ATree t = Tree (Stmt, Frame t)

frameLookup :: Frame t -> String -> Maybe t
frameLookup f s = symTables f
    & fmap (M.lookup s)
    & foldMap First
    & getFirst

newftree :: Stmt -> StmtA (Frame TType) ()
newftree stmt = go (Frame []) stmt where
    go f s = case s of
        SBlock () dd ss -> let dict = M.fromList [ (v, t) | Decl t v <- dd ]
                               f' = Frame (dict:symTables f)
                            in SBlock f' dd (fmap (go f') ss)
        SIf () b s1 s2 -> SIf f b (go f s1) (go f s2)
        SDoWhile () b s1 -> SDoWhile f b (go f s1)
        SWhile () b s1 -> SWhile f b (go f s1)
        SAssign () lv e -> SAssign f lv e
        SBreak -> SBreak

findtypes :: (Monad m, Alternative m) => StmtA (Frame TType) () -> m (StmtA (Frame TType) TType)
findtypes = go where
    go stmt = case stmt of
        SBlock f dd ss -> (SBlock f dd) <$> (traverse go ss)
        SIf f b s1 s2 -> (SIf f) <$> (typedExpr f b) <*> (go s1) <*> (go s2)
        SDoWhile f b s1 -> (SDoWhile f) <$> (typedExpr f b) <*> (go s1)
        SWhile f b s1 -> (SWhile f) <$> (typedExpr f b) <*> (go s1)
        SAssign f lv e -> (SAssign f) <$> (golv f lv) <*> (typedExpr f e) -- i2f and f2i ??
        SBreak -> pure SBreak
    golv f lvalue = case lvalue of
                    LVar v -> pure (LVar v)
                    LArr lv e -> LArr <$> (golv f lv) <*> (typedExpr f e)

ftree :: Stmt -> ATree TType
ftree s = let t1 = stree s
              t3 = fmap (id &&& (\m -> Frame [symtab m])) t1
            in obeyParents t3 where
            symtab :: Stmt -> M.Map String TType
            symtab (SBlock () dd _) = M.fromList (map (\(Decl t v) -> (v,t)) dd)
            symtab _ = M.empty

stree :: Stmt -> Tree Stmt
stree s@(SBlock () _ ss) = Node s (map stree ss)
stree s@(SIf () _ s1 s2) = Node s (map stree [s1,s2])
stree s@(SWhile () _ s1) = Node s (map stree [s1])
stree s@(SDoWhile () _ s1) = Node s (map stree [s1])
stree x = Node x []

obeyParents :: ATree t -> ATree t
obeyParents = paratree f where
            f f1 (s,(Frame (m:_))) = (s,Frame (m:symTables (snd f1)))

paratree :: (a -> a -> a) -> Tree a -> Tree a
-- f ottaa vanhemman ja lapsen, ja sijoittaa tuloksen lapseen
paratree f (Node a ts) = let ts1 = map child ts
                             ts2 = map (paratree f) ts1
                           in Node a ts2 where
                           child (Node a1 tt) = Node (f a a1) tt

-- yleinen tyyppi, voi olla esim Maybe TType tai Either String TType
exprType :: (Monad m, Alternative m) => Frame TType -> Expr -> m TType 
exprType f e = do
    te <- typedExpr f e
    return $ getTag te

typedExpr :: (Monad m, Alternative m) => Frame TType -> Expr -> m (ExprAnn TType)
typedExpr _ (ENum () x) = pure $ ENum TInt x
typedExpr _ (EReal () x) = pure $ EReal TFloat x
typedExpr _ (EBool () x) = pure $ EBool TBool x
typedExpr f (EFetch () v) = case frameLookup f v of
                                Just t -> return $ EFetch t v
                                Nothing -> fail ("Symbol not found: " ++ v)
typedExpr f (EArrayInd _ x ind) = do
                                      te1 <- typedExpr f x
                                      te2 <- typedExpr f ind -- tarkistetaan, onko indeksi int
                                      TArr t _ <- pure $ getTag te1
                                      TInt <- pure $ getTag te2
                                      return $ EArrayInd t te1 te2
typedExpr f (EUn () "!" e1) = do
                            te1 <- typedExpr f e1
                            guard $ getTag te1 == TBool
                            return $ EUn TBool "!" te1
typedExpr f (EUn () "-" e1) = do
                            te1 <- typedExpr f e1
                            let t = getTag te1
                            guard $ t `elem` [TInt, TBool]
                            return $ EUn t "-" te1
typedExpr f (EBin _ op e1 e2) 
    | op `elem` ["+","-","*","/"] = do -- t‰ss‰ voi olla int ja float, mutta vertailussa pit‰‰ olla sama tyyppi
                                      te1 <- typedExpr f e1
                                      te2 <- typedExpr f e2
                                      let t1 = getTag te1
                                          t2 = getTag te2
                                      guard $ all (`elem` [TInt,TFloat]) $ [t1,t2]
                                      let t = if t1 == t2 then t1 else TFloat
                                      te1 <- pure $ if (t1, t) == (TInt, TFloat) then EUn TFloat "i2f" te1 else te1
                                      te2 <- pure $ if (t2, t) == (TInt, TFloat) then EUn TFloat "i2f" te2 else te2
                                      return $ EBin t op te1 te2
    | op `elem` ["==","!="] = do
                                      te1 <- typedExpr f e1
                                      te2 <- typedExpr f e2
                                      guard (getTag te1 == getTag te2 && getTag te1 `elem` [TInt,TFloat,TBool,TChar])
                                      return $ EBin TBool op te1 te2
    | op `elem` ["<",">","<=",">="] = do
                                      te1 <- typedExpr f e1
                                      te2 <- typedExpr f e2
                                      let t1 = getTag te1
                                          t2 = getTag te2
                                      guard (t1 == t2 && t1 `elem` [TInt,TFloat])
                                      return $ EBin TBool op te1 te2
    | op `elem` ["||","&&"] = do
                                      te1 <- typedExpr f e1
                                      te2 <- typedExpr f e2
                                      guard $ getTag te1 == TBool
                                      guard $ getTag te2 == TBool
                                      return $ EBin TBool op te1 te2


checkTypes :: ATree TType -> Either String ()
checkTypes decorTree = let 
                   helper (s@(SAssign () lvalue expr),f) = do
                                                    t1 <- exprType f (lval2expr lvalue)
                                                    t2 <- exprType f expr
                                                    case (t1,t2) of
                                                       _ | t1 == t2 || all (`elem` [TInt,TFloat]) [t1,t2] -> return ()
                                                         | otherwise -> fail $ "Expression " ++ show expr ++ " has type " ++
                                                                           show t1 ++ ", variable " ++ show lvalue ++
                                                                           " has incompatible type " ++ show t2 ++
                                                                           " in statement " ++ show s
                   helper (SBlock () _ _, _) = return () -- ei tossa oo mit√§√§
                   helper (s@(SIf () b _ _),f) = checkBool b s f
                   helper (s@(SWhile () b _),f) = checkBool b s f
                   helper (s@(SDoWhile () b _),f) = checkBool b s f
                   helper (SBreak , _) = return ()
                   checkBool b s f = do
                                t1 <- exprType f b
                                case t1 of
                                  TBool -> return ()
                                  _ -> fail $ "Test must have boolean type, has " ++ show t1 ++ 
                                              " in statement " ++ show s
                  in F.forM_ decorTree helper
