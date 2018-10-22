import Parser
import StaticAnalysis
import CodeGen
import qualified CodeGen2 as C2
import System.Environment
import System.IO
import qualified Data.Map as M
import Control.Monad.State
import LLVM.Pretty
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Foldable

main :: IO ()
main = do
    args <- getArgs
    str <- readFile (args!!0)
    let s = wParse str
        t1 = newftree s
        Just t2 = findtypes t1
        ((), koodi) = runIRBuilder emptyIRBuilder $ do 
                ensureBlock
                t3 <- C2.newnewvars t2
                C2.stmt t3
                zero <- int32 0
                ret zero
    putStrLn "define i32 @main() {\n"
    traverse_ (T.putStrLn . ppll) koodi
    putStrLn "}\n"
