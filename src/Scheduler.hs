module Scheduler where

import Prelude hiding (foldl,foldr,concat)
import Data.Foldable
import NetLAST
import DepGraph
import NetLAST

varUsedIn :: Expr BaseArg -> [Ident]
varUsedIn (Ram _ ra _ _ _) = argToVar ra
varUsedIn x = foldMap argToVar x

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
