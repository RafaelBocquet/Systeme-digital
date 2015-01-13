{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, MultiParamTypeClasses, ScopedTypeVariables #-}

module Magma.Circuit where

import Prelude hiding (id, (.), and, or, zip)
import Data.Traversable
import Data.Foldable hiding (and, or)
import Data.Functor
import Data.Monoid
import Data.Type.Equality
import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Functor.Identity
import Control.Monad.State

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified GHC.TypeLits

-- Nat
data Nat = Z | S Nat
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

class NatSingleton n where
  natSingleton :: SNat n
instance NatSingleton Z where
  natSingleton = SZ
instance NatSingleton n => NatSingleton (S n) where
  natSingleton = SS (natSingleton)
  
type family (N n) where
  N 0       = Z
  N n = S (N (n GHC.TypeLits.- 1))

type family n + m where
  n + Z   = n
  n + S m = S (n + m)

type family n * m where
  n * Z   = Z
  n * S m = (n * m) + n

type family P2 n where
  P2 Z     = S Z
  P2 (S n) = P2 n + P2 n

-- Zippable
class Zippable f where
  zip :: f a -> f b -> f (a, b)

-- Data types for the circuit
type Wire = Int

data Vec n a where
  VNil  :: Vec n a
  VCons :: a -> Vec n a -> Vec (S n) a
  
viewVCons :: Vec (S n) a -> (a, Vec n a)
viewVCons (VCons a as) = (a, as)
pattern a :> as <- (viewVCons -> (a, as))

-- splitVec :: SNat n -> SNat m -> Vec (m + n) a -> (Vec n a, Vec m a)
-- splitVec SZ     m v            = (VNil, v)
-- splitVec (SS n) m (VCons a as) =
--   let (b, c) = splitVec n m as
--   in (VCons a b, c)

class SplitVec n where
  splitVec :: Vec (m + n) a -> (Vec n a, Vec m a)
instance SplitVec Z where
  splitVec v = (VNil, v)
instance SplitVec n => SplitVec (S n) where
  splitVec (VCons a as) =
    let (b, c) = splitVec as
    in (VCons a b, c)

pattern a :| b <- (splitVec -> (a, b))

deriving instance Show a => Show (Vec n a)
instance Functor (Vec n) where
  fmap f VNil         = VNil
  fmap f (VCons a as) = f a `VCons` fmap f as
instance Foldable (Vec n) where
  foldMap f (VCons a as) = f a <> foldMap f as
instance Traversable (Vec n) where
  traverse f VNil         = pure VNil
  traverse f (VCons a as) = VCons <$> f a <*> traverse f as
instance Zippable (Vec n) where
  zip VNil VNil                 = VNil
  zip (VCons a as) (VCons b bs) = (a, b) `VCons` zip as bs


-- The circuit monad and type
data Equation = EAnd Wire Wire
              | EOr Wire Wire
              | EXor Wire Wire
              | ENand Wire Wire
              | ENot Wire
              | EConst Bool
              | EMux Wire Wire Wire

              | EReg Wire

data CircuitState = CircuitState
                    { circuitFresh     :: Wire
                    , circuitEquations :: Map Wire Equation
                    }
                    
newtype CircuitMonad a = CircuitMonad { unCircuitMonad :: State CircuitState a }
                         deriving (Functor, Applicative, Monad, MonadState CircuitState)

newtype Circuit a b = Circuit { unCircuit :: Kleisli CircuitMonad a b }
                      deriving (Category, Arrow)

newWire :: CircuitMonad Wire
newWire = do
  i <- circuitFresh <$> get
  modify $ \s -> s { circuitFresh = i + 1 }
  return i

addEquation :: Equation -> CircuitMonad Wire
addEquation e = do
  w <- newWire
  modify $ \s -> s { circuitEquations = Map.insert w e (circuitEquations s) }
  return w

-- Primitives
-- We have to use monadic code here

and2, or2, xor2, nand2 :: Circuit (Wire, Wire) Wire
and2  = Circuit . Kleisli $ \(a, b) -> addEquation (EAnd a b)
or2   = Circuit . Kleisli $ \(a, b) -> addEquation (EOr a b)
xor2  = Circuit . Kleisli $ \(a, b) -> addEquation (EXor a b)
nand2 = Circuit . Kleisli $ \(a, b) -> addEquation (ENand a b)

not1 :: Circuit Wire Wire
not1 = Circuit . Kleisli $ \a -> addEquation (ENot a)

mux3 :: Circuit (Wire, Wire, Wire) Wire
mux3 = Circuit . Kleisli $ \(a, b, c) -> addEquation (EMux a b c)


const0 :: Circuit Bool Wire
const0 = Circuit . Kleisli $ \c -> addEquation (EConst c)

reg :: (Wire -> Circuit a (Wire, b)) -> Circuit a b
reg f = Circuit . Kleisli $ \a -> do
  w <- newWire
  (w', b) <- runKleisli (unCircuit $ f w) a
  modify $ \s -> s { circuitEquations = Map.insert w (EReg w') (circuitEquations s) }
  return b


-- ...
class Muxable a where
  mux :: Circuit (Wire, a, a) a
instance Muxable Wire where
  mux = mux3
instance Muxable a => Muxable (Vec Z a) where
  mux = proc _ -> returnA -< VNil
instance (Muxable a, Muxable (Vec n a)) => Muxable (Vec (S n) a) where
  mux = proc (a, b :> bs, c :> cs) -> do
    d       <- mux -< (a, b, c)
    ds      <- mux -< (a, bs, cs)
    returnA -< d `VCons` ds

class Select n where
  select :: Muxable a => Circuit (Vec (P2 n) a, Vec n Wire) a
instance Select Z where
  select = proc (a :> VNil, _) -> returnA -< a
instance (SplitVec (P2 n), Select n) => Select (S n) where
  select = proc (b :| c, a :> as) -> do
    b' <- select -< (b, as)
    c' <- select -< (c, as)
    mux -< (a, b', c')
