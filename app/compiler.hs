{-# LANGUAGE ScopedTypeVariables #-}
import Parser
import StaticAnalysis
import AbstractSyntax
import qualified CodeGen2 as C2
import System.Environment
import Control.Monad.State
import Control.Monad.Except
import LLVM.Pretty
import LLVM.AST(Name)
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import qualified Data.Text.Lazy.IO as T
import Data.Foldable

main :: IO ()
main = do
    args <- getArgs
    str <- readFile (args!!0)
    let s = wParse str
        t1 = newftree s
        ((result, _), koodi) = runIRBuilder emptyIRBuilder $ flip runStateT [] $ runExceptT $ thing t1
    case result of
      Right () -> do
        putStrLn "define i32 @main() {\n"
        traverse_ (T.putStrLn . ppll) koodi
        putStrLn "}\n"
      Left message -> putStrLn message

thing :: StmtA (Frame TType) () -> ExceptT String (StateT [Name] IRBuilder) ()
thing t1 = do
    t2 <- findtypes t1
    ensureBlock
    t3 <- C2.newnewvars t2
    C2.stmt t3
    zero <- int32 0
    ret zero
