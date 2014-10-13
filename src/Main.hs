import Lexer
import Parser
import System.Environment
import System.IO

main =
  do file:_ <- getArgs
     handle <- openFile file ReadMode
     content <- hGetContents handle
     --print content
     print $ (alexScanTokens content)
     print "****"
     print $ parse $ (alexScanTokens content)
     return ()
