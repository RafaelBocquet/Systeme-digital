module Scheduler where

--import Parser
import NetLAST
import DepGraph
import Data.List
import NetLAST

varUsedIn :: Expr BaseArg -> [Ident]
varUsedIn (BOp _ a b) = (argToVar a) ++ (argToVar b)
varUsedIn (Select _ a) = (argToVar a)
varUsedIn (Concat a b) = (argToVar a) ++ (argToVar b)
varUsedIn (Slice  _ _ a) = argToVar a
varUsedIn (Id a) = argToVar a
varUsedIn Input = []
varUsedIn (Ram _ a b c d) = concatMap argToVar [a]
varUsedIn (Rom _ a) = argToVar a
varUsedIn (Not a) = argToVar a


argToVar :: BaseArg -> [Ident]
argToVar (BVar ident) = [ident]
argToVar _ = []

addDepOne :: BaseEqtn -> Graph -> Graph
addDepOne (ident, Reg a) =
  foldl (\f var -> \g -> f (addEdge g var ident)) id $
  argToVar a
addDepOne (ident,expr) =
  foldl (\f v -> \g -> f (addEdge g ident v)) id $
  varUsedIn expr
  
generateDepGraph :: NetL  -> Graph
generateDepGraph net =
  foldl' (flip ($)) (emptyGraph) $
  map addDepOne $
  eqtns net


scheduler :: NetL -> NetL
scheduler net =
  let g = generateDepGraph net in
  let l = topoSort g in
  net{eqtns= concat $ map (\label -> filter ((==label) . fst) (eqtns net)) l}
