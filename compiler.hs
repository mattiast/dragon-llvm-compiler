import Parser
import AbstractSyntax
import Static
import CodeGen
import System
import System.IO
import qualified Data.Map as M
import Control.Monad.State
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
    args <- getArgs
    str <- readFile (args!!0)
    let s = wParse str --parse parseExpr "" "(x[2*y + 1][3] + 1)*(y - 15)"
        t1 = ftree s
        koodiST = runStateT (do 
                t2 <- varNames t1
                koodi <- evalStmt t2
                let allocs = allocations t2
		    start = "define i32 @main() {\n"
		    end = "ret i32 0\n}\n"
                return (start ++ allocs ++ koodi ++ end)) M.empty
        ((koodi,_),_) = runState koodiST []
    case checkTypes t1 of
    	Right () -> putStr koodi
	Left s -> hPutStrLn stderr s
