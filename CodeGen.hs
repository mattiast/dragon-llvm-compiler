module CodeGen where
import StaticAnalysis
import AbstractSyntax
import Control.Monad.State
import Data.Tree
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Map as M

type NameState t = StateT (M.Map String Int) (State [Int]) t

-- Toi generoi uuden muuttujan annetulla prefixillä
genvar :: Var -> NameState Var
genvar prefix = do
        tbl <- get
        put $ case M.lookup prefix tbl of
                Just i -> M.update (Just . (+1)) prefix tbl
                Nothing -> M.insert prefix 1 tbl
        tbl1 <- get
        let Just i = M.lookup prefix tbl1
        return $ prefix ++ show i
pop_while :: NameState ()
pop_while = do
        w:ws <- lift get
        lift (put ws)
push_while :: NameState ()
push_while = do
        tbl <- get
        let Just i = M.lookup "end_while" tbl
        ws <- lift get
        lift (put $ i:ws)
break_label :: NameState Var
break_label = do
        i:_ <- lift get
        return $ "end_while" ++ show i

--annetaan jokaiselle muuttujalle yksikäsitteinen nimi
varNames :: ATree Type -> NameState (ATree (Type,Var))
varNames t = let
        helper (Node (s,f) ts) = do
                f1 <- uniks f
                ts1 <- sequence [helper tt | tt <- ts]
                return $ Node (s,f1) ts1
        uniks :: Frame Type -> NameState (Frame (Type,Var))
        uniks (Frame table _)  = do
                tbl <- T.mapM (\t -> do{v <- genvar "%var"; return (t,v)}) table
                return $ Frame tbl Nothing
        in do
                tree <- helper t
                return $ obeyParents tree

renderType :: Type -> String
renderType TInt = "i32"
renderType TChar = "i8"
renderType TBool = "i1"
renderType TFloat = "float"
renderType (TArr t1 n) = "[ " ++ show n ++ " x " ++ renderType t1 ++ " ]"

-- muistin varaukset alkuun, tolle annetaan varNames:n tulos
allocations :: ATree (Type,Var) -> String
allocations t = let
        simpleAlloc (t,v) = v ++ " = alloca " ++ renderType t ++ "\n"
        t1 = fmap (M.fold (++) "" . M.map simpleAlloc . symTable . snd) t
        in F.fold t1


evalPtr :: Frame (Type,Var) -> LValue -> NameState (Var, String)
evalPtr f lvalue = do
        let (var,inds) = extractLValue lvalue
            Just (typ,varName) = frameLookup f var
        (vinds,ss) <- fmap unzip $ sequence [ evalExpr f i | i <- inds ]
        vptr <- genvar "%ptr"
        return $ (vptr, concat ss ++ unlines [vptr ++ " = getelementptr " ++
                                                 renderType typ ++ "* " ++ 
                                                 varName ++ ", i32 0" ++
                                                 concat [ ", i32 " ++ v | v <- vinds ] ])
        
binOpStr :: BinOp -> Type -> (Var,Var,Var) -> String
-- aritmeettiset
binOpStr op t        (r0,r1,r2)  = r0 ++ " = " ++ opStr ++ " " ++ renderType t ++ " " ++ r1 ++ ", " ++ r2 where
    opStr = case op of
            "+" | t == TInt -> "add"
            "-" | t == TInt -> "sub"
            "*" | t == TInt -> "mul"
            "+" | t == TFloat -> "fadd"
            "-" | t == TFloat -> "fsub"
            "*" | t == TFloat -> "fmul"
            "/" | t == TInt -> "sdiv"
            "/" | t == TFloat -> "fdiv"
            "&&" | t == TBool -> "and"
            "||" | t == TBool -> "or"

            "==" | t == TFloat -> "fcmp oeq"
            "!=" | t == TFloat -> "fcmp one"
            "<"  | t == TFloat -> "fcmp olt"
            "<=" | t == TFloat -> "fcmp ole"
            ">"  | t == TFloat -> "fcmp ogt"
            ">=" | t == TFloat -> "fcmp oge"

            "==" -> "icmp eq"
            "!=" -> "icmp ne"
            "<"  -> "icmp slt"
            "<=" -> "icmp sle"
            ">"  -> "icmp sgt"
            ">=" -> "icmp sge"

unOpStr :: UnOp -> Type -> (Var,Var) -> String
unOpStr "-" t@TInt (r0,r1)       = r0 ++ " = sub " ++ renderType t ++ " 0, " ++ r1
unOpStr "-" t@TFloat (r0,r1)       = r0 ++ " = fsub " ++ renderType t ++ " 0, " ++ r1
unOpStr "!" t@TBool (r0,r1) = r0 ++ " = xor " ++ renderType t ++ " true, " ++ r1
unOpStr "f2i" t@TFloat (r0,r1) = r0 ++ " = fptosi " ++ renderType t ++ " " ++ r1 ++ " to " ++ renderType TInt
unOpStr "i2f" t@TInt (r0,r1)   = r0 ++ " = sitofp " ++ renderType t ++ " " ++ r1 ++ " to " ++ renderType TFloat

evalExpr :: Frame (Type,Var) -> Expr -> NameState (Var, String)
evalExpr f expr@(EFetch lv) = do
        let Just resultType = exprType (fmap fst f) expr
        (ptr_reg, eval_ptr) <- evalPtr f lv
        v1 <- genvar "%reg"
        return $ (v1, eval_ptr ++ unlines [ v1 ++ " = load " ++ renderType resultType ++ "* " ++ ptr_reg])
evalExpr f (ENum n) = do
        v1 <- genvar "%reg"
        return $ (v1, unlines [v1 ++ " = add i32 " ++ show n ++ ", 0"])
evalExpr f (EReal n) = do
        v1 <- genvar "%reg"
        return $ (v1, unlines [v1 ++ " = fadd float " ++ show n ++ ", 0.0"])
evalExpr f (EBool b) = do
        v1 <- genvar "%reg"
        let val = if b then "true" else "false"
        return $ (v1, unlines [v1 ++ " = add i1 " ++ val ++ ", 0"])
evalExpr f (EBin op e1 e2) = do
        (v1,s1) <- evalExpr f e1
        (v2,s2) <- evalExpr f e2
        v3 <- genvar "%reg"
        let Just t1 = exprType (fmap fst f) e1
            Just t2 = exprType (fmap fst f) e2
	(c1,c2) <- case (t1,t2) of
		_ | t1 == t2 -> return (v1,v2)
		(TInt,TFloat) -> do
			r <- genvar "%cast"
			return (r,v2)
		(TFloat,TInt) -> do
			r <- genvar "%cast"
			return (v1,r)
	let eval_cast = case (t1,t2) of
                          _ | t1 == t2 -> ""
                          (TInt,TFloat) -> unOpStr "i2f" TInt (c1,v1) ++ "\n"
                          (TFloat,TInt) -> unOpStr "i2f" TInt (c2,v2) ++ "\n"
        return $ (v3, s1 ++ s2 ++ eval_cast ++
                      unlines [binOpStr op (if t1 == t2 then t1 else TFloat) (v3,c1,c2)])
evalExpr f (EUn op e1) = do
        (v1,s1) <- evalExpr f e1
        v2 <- genvar "%reg"
        let Just t1 = exprType (fmap fst f) e1
            operStr = unOpStr op t1 (v2,v1)
        return (v2, s1 ++ unlines [operStr])

evalStmt :: ATree (Type,Var) -> NameState String
evalStmt (Node (SIf b s1 s2, f) [t1,t2]) = do
        lbl_true <- genvar "if_true"
        lbl_false <- genvar "if_false"
        lbl_end <- genvar "end_if"
        (test_reg,eval_test) <- evalExpr f b
        code1 <- evalStmt t1
        code2 <- evalStmt t2
        return $ eval_test ++ 
                 "br i1 " ++ test_reg ++ ", label %" ++ lbl_true ++ ", label %" ++ lbl_false ++ "\n" ++
                 lbl_true ++ ":\n" ++
                 code1 ++
                 "br label %" ++ lbl_end ++ "\n" ++
                 lbl_false ++ ":\n" ++ 
                 code2 ++
                 "br label %" ++ lbl_end ++ "\n" ++
                 lbl_end ++ ":\n"
evalStmt (Node (SWhile b s1, f) [t1]) = do
        lbl_test <- genvar "test"
        lbl_begin <- genvar "begin_while"
        lbl_end <- genvar "end_while"
        (test_reg,eval_test) <- evalExpr f b
        push_while
        code1 <- evalStmt t1
        pop_while
        return $ "br label %" ++ lbl_test ++ "\n" ++
                 lbl_begin ++ ":\n" ++
                 code1 ++
		 "br label %" ++ lbl_test ++ "\n" ++
                 lbl_test ++ ":\n" ++
                 eval_test ++
                 "br i1 " ++ test_reg ++ ", label %" ++ lbl_begin ++ ", label %" ++ lbl_end ++ "\n" ++
                 lbl_end ++ ":\n"
evalStmt (Node (SDoWhile b s1, f) [t1]) = do
        lbl_test <- genvar "test"
        lbl_begin <- genvar "begin_while"
        lbl_end <- genvar "end_while"
        (test_reg,eval_test) <- evalExpr f b
        push_while
        code1 <- evalStmt t1
        pop_while
        return $ "br label %" ++ lbl_begin ++ "\n" ++
	         lbl_begin ++ ":\n" ++
                 code1 ++
		 "br label %" ++ lbl_test ++ "\n" ++
                 lbl_test ++ ":\n" ++
                 eval_test ++
                 "br i1 " ++ test_reg ++ ", label %" ++ lbl_begin ++ ", label %" ++ lbl_end ++ "\n" ++
                 lbl_end ++ ":\n"
evalStmt (Node (SBlock dd ss, f) tt) = do
        codes <- sequence [ evalStmt t | t <- tt ]
        return $ concat codes
evalStmt (Node (SAssign l e1, f) []) = do
        (val_reg, eval_e) <- evalExpr f e1
        (ptr_reg, eval_ptr) <- evalPtr f l
        let Just resultType = exprType (fmap fst f) e1
	    Just lvalueType = exprType (fmap fst f) (EFetch l)
	casted_reg <- if resultType == lvalueType then return val_reg else genvar "%reg"
	let eval_cast = case (lvalueType,resultType) of
		(tl,tr) | tl == tr -> ""
		(tl@TInt,tr@TFloat) -> unOpStr "f2i" tr (casted_reg,val_reg) ++ "\n"
		(tl@TFloat,tr@TInt) -> unOpStr "i2f" tr (casted_reg,val_reg) ++ "\n"
	
        return $ eval_e ++
                 eval_ptr ++
		 eval_cast ++
                 "store " ++ renderType lvalueType ++ " " ++ casted_reg ++ ", " ++ 
                 renderType lvalueType ++ "* " ++ ptr_reg ++ "\n"
evalStmt (Node (SBreak,f) []) = do
        lbl_break <- break_label
        return $ "br label %" ++ lbl_break ++ "\n"
