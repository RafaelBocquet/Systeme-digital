module DepGraph where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad.State
import Data.Maybe

type Label = String

type Graph = M.Map Label (S.Set Label)



addEdge :: Graph -> Label ->  Label -> Graph
addEdge graph from to =
  let children = fromMaybe S.empty (M.lookup from graph) in
  let newChildren = S.insert to children in 
  M.insert from newChildren graph
  
data Mark = NotVisited | Visited | InProgress


cycleDetectionFromWithState :: Graph -> Label -> State (M.Map Label Mark) Bool
cycleDetectionFromWithState graph l =
  let recur node =
        case node  of
          Visited -> return False
          InProgress -> return True
          NotVisited ->
            do
              let children = fromMaybe [] (S.toList <$> (M.lookup l graph)) 
              modify (M.insert l InProgress)
              resul <- foldl (liftM2 (||)) (return False) $ map (cycleDetectionFromWithState graph) children
              modify (M.insert l Visited)
              return resul
  in
   do marked <- get
      recur (fromMaybe NotVisited $ M.lookup l marked)
      
cycleDetection :: Graph -> Bool
cycleDetection graph =
  let cycles = map (cycleDetectionFromWithState graph) (M.keys graph) in
  evalState (foldl (liftM2 (||)) (return False) cycles) (M.empty)


testOk = foldl (\g (x,y) -> addEdge g (show x) (show y)) (M.empty) [(x,y) | x<-[1..100], y<-[x+1..101]]

testPrblm = addEdge testOk "50" "40"
       
       
     
  
