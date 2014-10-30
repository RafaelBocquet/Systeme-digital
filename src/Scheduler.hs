module Scheduler where

--import Parser
import NetLAST
import DepGraph
import Data.List

varUsedIn :: Expr BaseArg -> [Ident]
varUsedIn (BOp _ a b) = (argToVar a) ++ (argToVar b)
varUsedIn (Select _ a) = (argToVar a)
varUsedIn (Concat a b) = (argToVar a) ++ (argToVar b)
varUsedIn (Slice  _ _ a) = argToVar a
varUsedIn (Id a) = argToVar a

argToVar :: BaseArg -> [Ident]
argToVar (BVar ident) = [ident]
argToVar _ = []

addDepOne :: BaseEqtn -> Graph -> Graph
addDepOne (ident,expr) =
  foldl (\f v -> \g -> f (addEdge g ident v)) id $
  varUsedIn expr
  
generateDepGraph :: NetL BaseArg -> Graph
generateDepGraph net =
  foldl' (flip ($)) (emptyGraph) $
  map addDepOne $
  eqtns net

scheduler :: NetL BaseArg -> NetL BaseArg
scheduler net =
  let g = generateDepGraph net in
  let l = topoSort g in
  net{eqtns= concat $ map (\label -> filter ((==label) . fst) (eqtns net)) l}

