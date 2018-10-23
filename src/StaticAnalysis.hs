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
        SAssign f lv e -> do
            tlv <- golv f lv
            te <- typedExpr f e
            let tl = getLTag tlv
                tr = getTag te
            case (tl, tr) of
                _ | tl == tr -> return $ SAssign f tlv te
                (TFloat, TInt) -> pure $ SAssign f tlv (EUn TFloat "i2f" te)
                (TInt, TFloat) -> pure $ SAssign f tlv (EUn TInt "f2i" te)
        SBreak -> pure SBreak
    golv f lvalue = case lvalue of
                    LVar () v -> do
                        t <- glue $ frameLookup f v
                        pure (LVar t v)
                    LArr () lv e -> do
                        TArr t _ <- getLTag <$> golv f lv
                        LArr t <$> (golv f lv) <*> (typedExpr f e)

glue :: (Monad m, Alternative m) => Maybe a -> m a
glue (Just x) = pure x
glue Nothing = empty


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
    | op `elem` ["+","-","*","/"] = do -- tässä voi olla int ja float, mutta vertailussa pitää olla sama tyyppi
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
