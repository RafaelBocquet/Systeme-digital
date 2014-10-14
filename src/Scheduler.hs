module Scheduler where

import Parser
import DepGraph
import Data.List

varUsedIn :: Expr -> [Ident]
varUsedIn (BOp _ a b) = (argToVar a) ++ (argToVar b)
varUsedIn (Select _ a) = [a]
varUsedIn (Concat a b) = [a,b]
varUsedIn (Slice  _ _ a) = [a]
varUsedIn (Id a) = argToVar a

argToVar (Var id) = [id]
argToVar _ = []

addDepOne :: Eqtn -> (Graph -> Graph)
addDepOne (ident,expr) =
  foldl (\f var -> \g -> (addEdge g ident var)) id $
  varUsedIn expr
  
generateDepGraph :: NetL -> Graph
generateDepGraph net =
  foldl' (flip ($)) (emptyGraph) $
  map addDepOne $
  op net

scheduler :: NetL -> [Eqtn]
scheduler net =
  let g = generateDepGraph net in
  let l = topoSort g in
  concat $
  map (\l -> filter ((==l) . fst) (op net)) l

