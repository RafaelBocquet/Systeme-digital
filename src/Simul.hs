{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
module Simul where
  
import Lexer(Op(..))
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
import qualified Data.Map.Lazy as Ml


-- | The ReaderContent will serve as an environment in the 'ReaderT' monad.
data ReaderContent = Env { tab :: IOArray Int [Bool],
                           -- ^ An array that associates a List of Boolean to each variable number.
                           varNameAndSize :: Array Int (String,Int),
                           -- ^ An array used only for queries that associate to each number the corresponding variable
                           --name and size @(a : *size*)@ in the Var field of a .net file)
                           rams :: M.Map (Int,Int) (IOArray Int [Bool]),
                           roms :: Ml.Map (Int,Int) (Array Int [Bool])
                         }


type Action a = ReaderT ReaderContent IO a

-- * 'Action' builders

eqtnListToFn:: [IndexedEqtn] -> ReaderT ReaderContent IO [()]
eqtnListToFn  = mapM eqtnToFn 



eqtnToFn :: IndexedEqtn -> Action ()

eqtnToFn (input, Input) = 
  do Env {tab,varNameAndSize} <- ask
     lift $ query (varNameAndSize ! input) >>= writeArray tab input 
       where query (s,l) = do putStr $ s ++ " (" ++ show l ++ ") ? "
                              (input:_) <- liftM words getLine
                              return $ map (/= '0') input

eqtnToFn (ident,expr) =
  do Env {tab} <- ask
     resul <- exprToFn expr
     lift $ writeArray tab ident resul


-- | Converts the binary representation of a number using [Bool] to an Int
binToDec :: [Bool] -> Int
binToDec = foldl' (+) 0 . zipWith (\n b -> if b then n else 0) [2^n | n<-[0..]] . reverse


exprToFn :: Expr IndexedArg -> Action [Bool]
exprToFn (Rom x ra) = do Env {roms} <- ask
                         ra' <- argToVal ra
                         return $ (roms Ml.! x) ! (binToDec ra')
exprToFn (Ram x ra _ _ _) = do Env {rams} <- ask
                               ra' <- argToVal ra
                               lift $ readArray (rams Ml.! x) (binToDec ra')
exprToFn (Reg a) = argToVal a
exprToFn (Id a) =  argToVal a
exprToFn (Not a) = liftM (map not) (argToVal a)
exprToFn (Select n a) = liftM ((: []) . (!! n)) (argToVal a)
exprToFn (Concat x y) = liftM2 (++) (argToVal x) (argToVal y)
exprToFn (Slice n m x) = liftM (take m . drop n) (argToVal x) 
exprToFn (BOp op a b) = do aval:_ <- argToVal a
                           bval:_  <- argToVal  b
                           return [opToFn op aval bval]
exprToFn (Mux i th el) = do i':_ <- argToVal i
                            th':_ <- argToVal th
                            el':_ <- argToVal el
                            return $ [if i' then th' else el']

argToVal :: IndexedArg -> Action [Bool]
argToVal (IdConst b) = return [b]
argToVal (IdVar ident) =
  do Env {tab} <- ask
     lift $ readArray tab ident   

opToFn XOR = (/=)
opToFn AND = (&&)
opToFn NAND = \a b -> not (a && b)
opToFn OR = (||)


-- | Prints the indexes given as first argument on stdout
outputFn :: [Int] -> ReaderT ReaderContent IO ()
outputFn out =
  do Env {tab,varNameAndSize} <- ask
     mapMVoid
       (\ind ->lift $
               readArray tab ind >>= (showOutput . (,) (fst $ varNameAndSize ! ind)))
       out
     where 
       showOutput (id,val) = putStrLn $ id ++ " : " ++ (map (\x -> if x then '1' else '0') val)

       
{- | 'indexer' takes a NetL where variables are still identified by a string and returns three values :

 * First a 3-tuple containing (Input indexes, Eqtns with indexes, Outputs Indexes)
 * Second an array that will became the 'varNameAndSize' field of the @ReaderContent@
 * Third a map from the variables name to their index
-}
indexer ::  NetL -> (([Int],[IndexedEqtn],[Int]), Array Int (String,Int), M.Map String Int) 
indexer net =
  let (!indNames, !varsIndex) = buildVarIndex (var net) in
  let f arg = case arg of
        BConst n -> IdConst (n /= 0)
        BVar s -> IdVar (varsIndex M.! s) in
  let indexOf = (varsIndex M.!) in
  let g (s,expr) = (indexOf s, fmap f expr) in
  ( (map indexOf (inputs net), fmap g (eqtns net), map indexOf (outputs net) ) , indNames, varsIndex)


buildVarIndex :: [Atom] -> (Array Int (String,Int), M.Map String Int)
buildVarIndex vars =
  let lgth = length vars in
  let (indNamesAndSize, varsIndex) =
        unzip $
        zipWith ($) (map atomWorker vars) [1..] in
  (array (1,lgth) indNamesAndSize, M.fromList varsIndex)
  where
    atomWorker (Wire id) n = ((n, (id,1) ) , (id, n))
    atomWorker (Ribbon id size) n =((n, (id,size)), (id,n))




-- * Rams and Roms handlers

-- | Adds a ram to the @rams@ @Map@
addRam :: IndexedEqtn -> M.Map (Int, Int) (IOArray Int [Bool]) -> IO ( M.Map (Int, Int) (IOArray Int [Bool]) )
addRam (_, Ram (ads,wds) _  _ _ _) m =
  do ram <- newArray (0,2^ads - 1) (replicate wds False)
     return $ M.insert (ads,wds) ram m

-- | Handles writing in the @Ram@
ramFn :: IndexedEqtn -> ReaderT ReaderContent IO ()
ramFn (_,Ram x _ we wa c) = do Env {rams, tab} <- ask
                               we' <- argToVal we
                               when (we' == [True]) $
                                 do wa' <- argToVal wa
                                    c' <- argToVal c    
                                    lift $ writeArray (rams M.! x) (binToDec wa') c'
                                

-- | Adds a rom to the @roms@ @Map@
addRom :: IndexedEqtn -> Ml.Map (Int, Int) (Array Int [Bool]) -> IO ( Ml.Map (Int, Int) (Array Int [Bool]) )
addRom (_, Rom (ads,wds) _) m =
  do input <- readFile ((show ads)++"x"++(show wds)++".rom")
     let rom =  listArray (0,(2^ads) - 1) (map (map (== '1')) (lines input)) 
     return $ Ml.insert (ads,wds) rom m



-- * Simulator entry point and dirty hacks

-- | Takes a @NetL@ and returns a tuple containing the @Action ()@ representing a cycle and the initial @ReaderContent@
     
netLToFn :: NetL -> (Action (), IO ReaderContent)
netLToFn net =
  let ((ins, eqtns, outs),array,index) = indexer $ scheduler (preWorker net) in
  let ramEqtns = filter (\(_,expr) -> case expr of {Ram{} -> True; _ -> False}) eqtns in
  let rams =  foldl' (\m eqtn -> m >>= (addRam eqtn)) (return M.empty) ramEqtns in
  let romEqtns = filter (\(_,expr) -> case expr of {Rom _ _  -> True; _ -> False}) eqtns in
  let roms = foldl' (\m eqtn -> m >>= (addRom eqtn)) (return Ml.empty) romEqtns in
  (eqtnListToFn eqtns >> (mapMVoid ramFn ramEqtns) >> outputFn outs,
   (liftM3 $ \t rams roms-> Env {tab = t, varNameAndSize=array, rams=rams, roms=roms})
   (newArray (1,length (var net) + 1) [False]) rams roms
  ) 


-- | Home of the dirty hack (cf. README)
preWorker :: NetL -> NetL
preWorker net =
  let inputEqtns = concatMap (\id -> [(id,Input), ("VARINUTILE", Id (BVar id))]) (inputs net) in
    net{eqtns=(eqtns net) ++inputEqtns, var=(Wire "VARINUTILE"):(var net)}


-- | A type-restricted version of @mapM_@ that ensures that no values of type other than @()@ are discarded.
mapMVoid :: (Monad m) => (a -> m ()) -> [a] -> m ()
mapMVoid = mapM_

