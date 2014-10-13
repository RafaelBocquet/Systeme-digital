import Lexer
import Parser
import Scheduler
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
  do file: <- getArgs
     handle <- openfile file ReadMode
     content <- hGetContents handle
     let netl = parse $ (alexScanTokens content)
     print $ scheduler netl
