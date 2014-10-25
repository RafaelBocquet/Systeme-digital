{-# OPTIONS_GHC -XBangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Simul where
  
import Lexer
import Scheduler
import Parser
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.ST
import Control.Monad.Trans.Reader
import Data.Array.IO.Safe
import Data.Array.MArray
import Data.Array.IArray
import Data.Array.Base
import Data.List
import qualified Data.Map.Strict as M


opToFn XOR = (/=)
opToFn AND = (&&)
opToFn NAND = \a b -> not (a && b)
opToFn OR = (||)

--data Value = Simple Bool | Mult (Arra

data ReaderContent = Env { tab :: IOUArray Integer Bool,
                           varNum :: M.Map Ident Integer}
                     
type Action a = ReaderT ReaderContent IO a

argToVal :: Arg -> Action Bool
argToVal (Const n) = return (n /= 0)
argToVal (Var ident) =
  do Env {varNum, tab} <- ask
     lift $ readArray tab (varNum M.! ident)

eqtnToFn :: (Ident,Expr) -> Action ()
eqtnToFn (ident,expr) =
  do Env {tab, varNum} <- ask
     resul <- exprToFn expr
     lift $ writeArray tab (varNum M.! ident) resul

exprToFn :: Expr -> Action Bool
exprToFn (BOp op a b) =
  do Env {tab,varNum} <- ask
     aval <- argToVal a
     bval <- argToVal  b
     return $ opToFn op aval bval
exprToFn (Id a) = argToVal a

extractAtomName (Wire s) = s
extractAtomName (Ribbon s _) = s

mapVarNametoIntegers vars =
  M.fromList $
  zip (map extractAtomName vars) [(1::Integer)..]

eqtnListToFn:: [(Ident,Expr)] -> ReaderT ReaderContent IO [()]
eqtnListToFn  = mapM eqtnToFn 

outputFn :: [Ident] -> ReaderT ReaderContent IO ()
outputFn out =
  do Env {tab,varNum} <- ask
     mapMVoid
       (\s ->lift $
             readArray tab (varNum M.! s) >>= (showOutput . (,) s )  )
       out
     

inputFn :: [Ident] -> ReaderT ReaderContent IO ()
inputFn !inputs  =
  do Env {tab,varNum} <- ask
     mapMVoid
       (\s -> lift $
              query s >>= writeArray tab (varNum M.! s) )
       inputs

netLToFn :: NetL -> ReaderT ReaderContent IO ()
netLToFn !netL =
  inputFn (inp netL) >> (eqtnListToFn $! scheduler netL) >> outputFn (out netL)


initContent :: NetL -> IO ReaderContent
initContent netL =
  let v = var netL in
  liftM
  (\t -> Env {tab = t, varNum = mapVarNametoIntegers v})
  (newArray (1,genericLength v) False)

simul n net = liftM doSeq $
              sequence (replicate n $! (netLToFn net))

mapMVoid :: (Monad m) => (a -> m ()) -> [a] -> m ()
mapMVoid = mapM_



showOutput (id,val) = putStrLn $ id ++ " : " ++ (if val then "1" else "0")

doSeq :: [()] -> ()
doSeq = const ()

query :: String -> IO Bool
query s = do putStr $ s ++ " ? "
             (input:_) <- liftM words getLine
             return $ input /= "0"
