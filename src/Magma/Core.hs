{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows #-}

import Prelude hiding ((.), and, or)
import Data.Traversable
import Data.Foldable hiding (and, or)
import Data.Functor
import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Functor.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans
import qualified Data.Map.Strict as MS
infix 2 :-:


data Pair a =
  a :-: a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Pair where
  pure x = x :-: x
  (f :-: g) <*> (x :-: y) = (f x :-: g y) 
  _ *> x = x
  x <* _ = x


lift2Gate :: (Wire -> Wire -> Eqtn Wire) -> Circuit (Wire,Wire) Wire
lift2Gate f =
  Circuit $ Kleisli $ \(a,b) ->
  addEqtn (f a b)

duplicate2Gate :: Arrow f => f (a,b) c -> f (Pair a, Pair b) (Pair c)
duplicate2Gate c =
  proc ((x1 :-: x2), (y1 :-: y2)) -> do r1 <- c -< (x1,y1)
                                        r2 <- c -< (x2,y2)
                                        returnA -< r1 :-: r2
    

lift3Gate :: (Wire -> Wire -> Wire -> Eqtn Wire) -> Circuit (Wire,Wire,Wire) Wire
lift3Gate f =
  Circuit $ Kleisli $ \(a,b,c) ->
  addEqtn (f a b c)

duplicate3Gate :: Arrow f => f (a,b,c) d -> f (Pair a, Pair b, Pair c) (Pair d)
duplicate3Gate c =
  proc ((x1 :-: x2), (y1 :-: y2), (z1 :-: z2)) -> do
    r1 <- c -< (x1,y1,z1)
    r2 <- c -< (x2,y2,z2)
    returnA -< r1 :-: r2
    


type Wire = Integer

data Eqtn a = And a a
            | Or a a
            | Mux a a a
            | Xor a a
            | Nand a a
              deriving (Show,Eq)

type CEnv = StateT (MS.Map Wire (Eqtn Wire)) (State Wire) 

newtype Circuit a b = Circuit {runCircuit :: Kleisli CEnv a b}
                      deriving (Arrow, Category)


addEqtn :: Eqtn Wire -> CEnv Wire 
addEqtn e =
  do v <- freshWire
     modify (MS.insert v e)
     return v
  where
    freshWire =
      lift $
      do c <- get
         put (c+1)
         return c



class Logical a where
  and :: Circuit (a, a) a
  or :: Circuit (a,a) a
  xor :: Circuit (a,a) a
  mux :: Circuit (a,a,a) a


instance Logical Wire where
  and = lift2Gate And
  or = lift2Gate Or
  xor = lift2Gate Xor
  mux = lift3Gate Mux

instance Logical a => Logical (Pair a) where
  and = duplicate2Gate and
  or = duplicate2Gate or
  xor = duplicate2Gate xor
  mux = duplicate3Gate mux

lol :: Circuit (Wire,Wire,Wire) (Wire,Wire)
lol =
  proc (a,b,c) -> do r <- xor -< (a,b)
                     s <- and -< (r,c)
                     returnA -< (r,s)

getCircuitEqtn :: Circuit a b -> a -> MS.Map Wire (Eqtn Wire)
getCircuitEqtn c  init =
  let resultingState = (runKleisli . runCircuit $ c) init in
  fst $ runState  (execStateT resultingState (MS.empty)) 0 
