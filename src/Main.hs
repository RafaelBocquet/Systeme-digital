import Lexer
import Parser
import Scheduler
import Simul
import Control.Monad.Trans.Reader
import System.Environment
import System.IO

testmain1 =
  do file:_ <- getArgs
     handle <- openFile file ReadMode
     content <- hGetContents handle
     --print content
     print $ (alexScanTokens content)
     print "****"
     print $ parse $ (alexScanTokens content)
     return ()

main =
  do file:n:_ <- getArgs
     handle <- openFile file ReadMode
     content <- hGetContents handle
     let net = parse $! (alexScanTokens content)
     hSetBuffering stdout NoBuffering
     runReaderT (simul (read n) net) =<< (initContent net)
