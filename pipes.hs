import Control.Monad (unless)
import Pipes
import qualified Pipes.Prelude as P
import System.IO (isEOF)

echo = for P.stdinLn (lift . putStrLn)

main = runEffect echo
