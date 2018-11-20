{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module StaticAnalysis
    ( frameLookup
    , findtypes
    , newftree
    , Frame(..)
    , glue
    , guardWith
    ) where
import AbstractSyntax
import qualified Data.Map as M
import Control.Monad.Except
import Data.Function((&))
import Data.Monoid

newtype Frame t = Frame { symTables :: [M.Map String t]} deriving (Eq,Ord,Show, Functor) 

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

findtypes :: (MonadError String m) => StmtA (Frame TType) () -> m (StmtA (Frame TType) TType)
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
                _ -> throwError "weird types in assignment"
        SBreak -> pure SBreak
    golv f lvalue = case lvalue of
                    LVar () v -> do
                        t <- glue "error" $ frameLookup f v
                        pure (LVar t v)
                    LArr () lv e -> do
                        TArr t _ <- getLTag <$> golv f lv
                        LArr t <$> (golv f lv) <*> (typedExpr f e)

glue :: (MonadError String m) => String -> Maybe a -> m a
glue message mx = maybe (throwError message) pure mx

guardWith :: (MonadError String m) => String -> Bool -> m ()
guardWith message check = if check
                            then return ()
                            else throwError message

typedExpr :: (MonadError String m) => Frame TType -> Expr -> m (ExprAnn TType)
typedExpr _ (ENum () x) = pure $ ENum TInt x
typedExpr _ (EReal () x) = pure $ EReal TFloat x
typedExpr _ (EBool () x) = pure $ EBool TBool x
typedExpr f (EFetch () v) = case frameLookup f v of
                                Just t -> return $ EFetch t v
                                Nothing -> throwError ("Symbol not found: " ++ v)
typedExpr f (EArrayInd _ x ind) = do
                                      te1 <- typedExpr f x
                                      te2 <- typedExpr f ind -- tarkistetaan, onko indeksi int
                                      TArr t _ <- pure $ getTag te1
                                      TInt <- pure $ getTag te2
                                      return $ EArrayInd t te1 te2
typedExpr f (EUn () "!" e1) = do
                            te1 <- typedExpr f e1
                            guardWith "error" $ getTag te1 == TBool
                            return $ EUn TBool "!" te1
typedExpr f (EUn () "-" e1) = do
                            te1 <- typedExpr f e1
                            let t = getTag te1
                            guardWith "error" $ t `elem` [TInt, TBool]
                            return $ EUn t "-" te1
typedExpr _ (EUn () op _) = throwError $ "weird unary operation " ++ op
typedExpr f (EBin _ op e1 e2) 
    | op `elem` ["+","-","*","/"] = do -- tässä voi olla int ja float, mutta vertailussa pitää olla sama tyyppi
                                      te1 <- typedExpr f e1
                                      te2 <- typedExpr f e2
                                      let t1 = getTag te1
                                          t2 = getTag te2
                                      guardWith "error" $ all (`elem` [TInt,TFloat]) $ [t1,t2]
                                      let t = if t1 == t2 then t1 else TFloat
                                      ce1 <- pure $ if (t1, t) == (TInt, TFloat) then EUn TFloat "i2f" te1 else te1
                                      ce2 <- pure $ if (t2, t) == (TInt, TFloat) then EUn TFloat "i2f" te2 else te2
                                      return $ EBin t op ce1 ce2
    | op `elem` ["==","!="] = do
                                      te1 <- typedExpr f e1
                                      te2 <- typedExpr f e2
                                      guardWith "error" (getTag te1 == getTag te2 && getTag te1 `elem` [TInt,TFloat,TBool,TChar])
                                      return $ EBin TBool op te1 te2
    | op `elem` ["<",">","<=",">="] = do
                                      te1 <- typedExpr f e1
                                      te2 <- typedExpr f e2
                                      let t1 = getTag te1
                                          t2 = getTag te2
                                      guardWith "error" (t1 == t2 && t1 `elem` [TInt,TFloat])
                                      return $ EBin TBool op te1 te2
    | op `elem` ["||","&&"] = do
                                      te1 <- typedExpr f e1
                                      te2 <- typedExpr f e2
                                      guardWith "error" $ getTag te1 == TBool
                                      guardWith "error" $ getTag te2 == TBool
                                      return $ EBin TBool op te1 te2
    | otherwise = throwError $ "weird binary operation " ++ op
