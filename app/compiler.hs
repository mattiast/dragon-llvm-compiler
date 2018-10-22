import Parser
import StaticAnalysis
import CodeGen
import qualified CodeGen2 as C2
import System.Environment
import System.IO
import qualified Data.Map as M
import Control.Monad.State
import LLVM.Pretty
import qualified Data.Text.Lazy as T

main :: IO ()
main = do
    args <- getArgs
    str <- readFile (args!!0)
    let s = wParse str
        t1 = ftree s
        koodiST = runStateT (do 
                t2 <- varNames t1
                koodi <- evalStmt t2
                let allocs = C2.allocations t2
                    start = "define i32 @main() {\n"
                    end = "ret i32 0\n}\n"
                return (start ++ unlines (map (T.unpack . ppll) allocs) ++ koodi ++ end)) M.empty
        ((koodi,_),_) = runState koodiST []
    case checkTypes t1 of
        Right () -> putStr koodi
        Left str -> hPutStrLn stderr str
