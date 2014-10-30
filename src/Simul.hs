{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
module Simul where
  
import Lexer
import Scheduler
import NetLAST
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Array.IO.Safe
import Data.Array.MArray
import Data.Array.IArray
import Data.Array.Base
import Data.List
import qualified Data.Map.Strict as M


--data Value = Simple Bool | Mult (Arra

data ReaderContent = Env { tab :: IOArray Int [Bool],
                           varNameAndSize :: Array Int (String,Int)}


type Action a = ReaderT ReaderContent IO a

{- Action builders : build an Action () from an equation list -}

eqtnListToFn:: [IndexedEqtn] -> ReaderT ReaderContent IO [()]
eqtnListToFn  = mapM eqtnToFn 

eqtnToFn :: IndexedEqtn -> Action ()
eqtnToFn (ident,expr) =
  do Env {tab, varNameAndSize} <- ask
     resul <- exprToFn expr
     lift $ writeArray tab ident resul


exprToFn :: Expr IndexedArg -> Action [Bool]

exprToFn (Id a) =  argToVal a
exprToFn (Select n a) = liftM ((: []) . (!! n)) (argToVal a)
exprToFn (Concat x y) = liftM2 (++) (argToVal x) (argToVal y)
exprToFn (Slice n m x) = liftM (take m . drop n) (argToVal x) 
exprToFn (BOp op a b) = do Env {tab,varNameAndSize} <- ask
                           aval:_ <- argToVal a
                           bval:_  <- argToVal  b
                           return [opToFn op aval bval]


argToVal :: IndexedArg -> Action [Bool]
argToVal (IdConst b) = return [b]
argToVal (IdVar ident) =
  do Env {varNameAndSize , tab} <- ask
     lift $ readArray tab ident   

opToFn XOR = (/=)
opToFn AND = (&&)
opToFn NAND = \a b -> not (a && b)
opToFn OR = (||)

{- Indexer : turns a NetL BaseArg to a NetL IndexedArgs -}

indexer ::  NetL BaseArg -> (([Int],[IndexedEqtn],[Int]), Array Int (String,Int))
indexer net =
  let (!indNames, !varsIndex) = buildVarIndex (var net) in
  let f arg = case arg of
        BConst n -> IdConst (n /= 0)
        BVar s -> IdVar (varsIndex M.! s) in
  let indexOf = (varsIndex M.!) in
  let g (s,expr) = (indexOf s, fmap f expr) in
  ( (map indexOf (inputs net), fmap g (eqtns net), map indexOf (outputs net) ) , indNames)


buildVarIndex :: [Atom] -> (Array Int (String,Int), M.Map String Int)
buildVarIndex vars =
  let lgth = length vars in
  let (indNamesAndSize, varsIndex) =
        unzip $
        zipWith ($) (map extractAtomName vars) [1..] in
  (array (1,lgth) indNamesAndSize, M.fromList varsIndex)


extractAtomName :: Atom -> Int -> ((Int,(Ident,Int)),(Ident,Int))
extractAtomName (Wire id) n = ((n, (id,1) ) , (id, n))
extractAtomName (Ribbon id size) n =((n, (id,size)), (id,n))






outputFn :: [Int] -> ReaderT ReaderContent IO ()
outputFn out =
  do Env {tab,varNameAndSize} <- ask
     mapMVoid
       (\ind ->lift $
               readArray tab ind >>= (showOutput . (,) (fst $ varNameAndSize ! ind)))
       out
     

inputFn :: [Int] -> ReaderT ReaderContent IO ()
inputFn !inputs  =
  do Env {tab,varNameAndSize} <- ask
     mapMVoid
       (\ind -> lift $
                query (varNameAndSize ! ind) >>= writeArray tab ind )

       inputs

netLToFn :: NetL BaseArg -> (ReaderT ReaderContent IO (), IO ReaderContent)
netLToFn net =
  let ((ins, eqtns, outs),array) = indexer $ scheduler net in
  (inputFn (ins) >> (eqtnListToFn eqtns) >> outputFn (outs),
   (liftM $ \t -> Env {tab = t, varNameAndSize=array}) $
   newArray (1,length (var net)) [False]
  ) 


-- initContent :: NetL -> IO ReaderContent
-- initContent netL =
--   let v = var netL in
--   liftM
--   (\t -> Env {tab = t, varNameAndSize = mapVarNametoIntegers v})
--   (newArray (1,genericLength v) False)


mapMVoid :: (Monad m) => (a -> m ()) -> [a] -> m ()
mapMVoid = mapM_


showOutput :: (String, [Bool]) -> IO ()
showOutput (id,val) = putStrLn $ id ++ " : " ++ (map (\x -> if x then '1' else '0') val)

doSeq :: [()] -> ()
doSeq = const ()

query :: (String,Int) -> IO [Bool]
query (s,l) = do putStr $ s ++ " (" ++ show l ++ ") ?"
                 (input:_) <- liftM words getLine
                 return $ map (/= '0') input
