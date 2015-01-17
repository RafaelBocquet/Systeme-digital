{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, MultiParamTypeClasses, ScopedTypeVariables #-}

module Magma.Circuit where

import Prelude hiding (id, (.), and, or, zip, foldr1, concat)
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

import Magma.Nat
import Magma.Vec

-- Data types for the circuit
data Wire = WWire Int
          | WConst Bool

-- The circuit monad and type
data Equation = EAnd Wire Wire
              | EOr Wire Wire
              | EXor Wire Wire
              | ENand Wire Wire
              | ENot Wire
              | EMux Wire Wire Wire
                -- Rom and Ram : (2^28) 32-bits blocks
                -- Reduce address space ?
                -- Rom : Read Addr
              | ERom (Vec (N 28) Wire) 
                -- Ram : Read Addr, Write Addr, Write Value, Write Enable
              | ERam (Vec (N 28) Wire) (Vec (N 28) Wire) (Vec (N 32) Wire) Wire
                -- Select is only used internally
              | ESelect Wire Int
              | EReg Wire

data CircuitState = CircuitState
                    { circuitFresh     :: Int
                    , circuitEquations :: Map Int Equation
                    }

emptyCircuitState :: CircuitState
emptyCircuitState = CircuitState 0 Map.empty
                    
newtype CircuitMonad a = CircuitMonad { unCircuitMonad :: State CircuitState a }
                         deriving (Functor, Applicative, Monad, MonadState CircuitState)

newtype Circuit a b = Circuit { unCircuit :: Kleisli CircuitMonad a b }
                      deriving (Category, Arrow, ArrowApply)

-- TODO : circuit input / output
runCircuit :: Circuit () () -> String
runCircuit (Circuit c) =
  let eqs = circuitEquations . flip execState emptyCircuitState . unCircuitMonad $ runKleisli c ()
      inputs     = [] :: [Int]
      outputs    = [] :: [Int]
      vars       = Map.keys eqs
      varname x  = "V" ++ show x
      commasep   = foldr1 (\a b -> a ++ ", " ++ b)
      printWire (WWire a) = varname a
      printEquation (EAnd a b)     = "AND " ++ printWire a ++ " " ++ printWire b
      printEquation (EOr a b)      = "OR " ++ printWire a ++ " " ++ printWire b
      printEquation (EXor a b)     = "XOR " ++ printWire a ++ " " ++ printWire b
      printEquation (ENand a b)    = "NAND " ++ printWire a ++ " " ++ printWire b
      printEquation (ENot a)       = "NOT " ++ printWire a
      printEquation (EMux a b c)   = "MUX " ++ printWire a ++ " " ++ printWire b ++ " " ++ printWire c
      -- need to concat 28 wires
--      printEquation (ERom a)       = "ROM " ++ printWire a
--      printEquation (ERam a b c d) = "RAM " ++ printWire a ++ " " ++ printWire b ++ " " ++ printWire c ++ " " ++ printWire d
      printEquation (ESelect a b)  = "SELECT " ++ show b ++ " " ++ printWire a
      printEquation (EReg a)       = "REG " ++ printWire a
  in concat
     [ "INPUT"
     , commasep (varname <$> inputs)
     , "\nOUTPUT"
     , commasep (varname <$> outputs)
     , "\nVAR"
     , commasep (varname <$> vars)
     , "\nIN\n"
     , concat (fmap (\(w, e) -> varname w ++ " = " ++ printEquation e ++ "\n") (Map.toList eqs)) 
     ]
     

freshWire :: CircuitMonad Wire
freshWire = do
  i <- circuitFresh <$> get
  modify $ \s -> s { circuitFresh = i + 1 }
  return (WWire i)

addEquation :: Equation -> CircuitMonad Wire
addEquation e = do
  WWire w <- freshWire
  modify $ \s -> s { circuitEquations = Map.insert w e (circuitEquations s) }
  return (WWire w)

-- Primitives
-- We have to use monadic code here
-- TODO : cases to reduce constants
-- ex. and2 (True, a) ~ a

and2, or2, xor2, nand2 :: Circuit (Wire, Wire) Wire
and2  = Circuit . Kleisli $ \(a, b) -> addEquation (EAnd a b)
or2   = Circuit . Kleisli $ \(a, b) -> addEquation (EOr a b)
xor2  = Circuit . Kleisli $ \(a, b) -> addEquation (EXor a b)
nand2 = Circuit . Kleisli $ \(a, b) -> addEquation (ENand a b)

not1 :: Circuit Wire Wire
not1 = Circuit . Kleisli $ \a -> addEquation (ENot a)

mux3 :: Circuit (Wire, Wire, Wire) Wire
mux3 = Circuit . Kleisli $ \(a, b, c) -> addEquation (EMux a b c)

-- These three primitives are more complex, because of the possible cyclic dependencies and of the lack of multiwire equations

reg :: (Wire -> Circuit a (Wire, b)) -> Circuit a b
reg f = Circuit . Kleisli $ \a -> do
  WWire w <- freshWire
  (w', b) <- runKleisli (unCircuit $ f (WWire w)) a
  modify $ \s -> s { circuitEquations = Map.insert w (EReg w') (circuitEquations s) }
  return b

rom :: Circuit (Vec (N 28) Wire) (Vec (N 32) Wire)
rom = Circuit . Kleisli $ \a -> do
  WWire w  <- freshWire
  modify $ \s -> s { circuitEquations = Map.insert w (ERom a) (circuitEquations s) }
  ws <- traverse
        (\i -> do
            WWire w' <- freshWire
            modify $ \s -> s { circuitEquations = Map.insert w' (ESelect (WWire w) i) (circuitEquations s) }
            return (WWire w')
        )
        toNameVec
  return ws

ram :: (Vec (N 32) Wire -> Circuit a (Vec (N 28) Wire, Vec (N 32) Wire, Wire, b)) -> Circuit (Vec (N 28) Wire, a) b
ram f = Circuit . Kleisli $ \(ra, a) -> do
  WWire w  <- freshWire
  ws <- traverse
        (\i -> do
            w' <- freshWire
            return (i, w')
        )
        toNameVec
  (wa, wd, we, b) <- runKleisli (unCircuit $ f (fmap snd ws)) a
  modify $ \s -> s { circuitEquations = Map.insert w (ERam ra wa wd we) (circuitEquations s) }
  traverse
    (\(i, WWire w') -> do 
        modify $ \s -> s { circuitEquations = Map.insert w' (ESelect (WWire w) i) (circuitEquations s) }
    )
    ws
  return b

-- -- ...

class RegisterLike r where
  registerLike :: (r -> Circuit a (r, b)) -> Circuit a b
instance RegisterLike Wire where
  registerLike = reg
instance RegisterLike (Vec Z r) where
  registerLike f = arr snd . f VNil
instance (RegisterLike r, RegisterLike (Vec n r)) => RegisterLike (Vec (S n) r) where
  registerLike f = registerLike
                   $ \a -> registerLike
                           $ \as -> proc i -> do
                             (b :> bs, o) <- f (a `VCons` as) -< i
                             returnA -< (bs, (b, o))                        

class Muxable a where
  mux :: Circuit (Wire, a, a) a
instance Muxable Wire where
  mux = mux3
instance (Muxable a, Muxable b) => Muxable (a, b) where
  mux = proc (a, (b0, c0), (b1, c1)) -> do
    b <- mux -< (a, b0, b1)
    c <- mux -< (a, c0, c1)
    returnA -< (b, c)
instance Muxable a => Muxable (Vec Z a) where
  mux = proc _ -> returnA -< VNil
instance (Muxable a, Muxable (Vec n a)) => Muxable (Vec (S n) a) where
  mux = proc (a, b :> bs, c :> cs) -> do
    d       <- mux -< (a, b, c)
    ds      <- mux -< (a, bs, cs)
    returnA -< d `VCons` ds

class Select n where
  select :: Muxable a => Circuit (Vec (P2 n) a, Vec n Wire) a
  -- May have found a GHC bug when pattern matching with (a :> VNil)
instance Select Z where
  select = proc (a :> _, _) -> returnA -< a
instance (NatSingleton (P2 n), Select n) => Select (S n) where
  select = proc (b :| c, a :> as) -> do
    b' <- select -< (b, as)
    c' <- select -< (c, as)
    mux -< (a, b', c')

class Default a where
  defaultValue :: a
instance Default Wire where
  defaultValue = WConst False
instance Default (Vec Z a) where
  defaultValue = VNil
instance (Default a, Default (Vec n a)) => Default (Vec (S n) a) where
  defaultValue = VCons defaultValue defaultValue

{- needs to be implemented from BDD -}
{- Maybe -> Nothing = dont care ? -}
{- Only DontCares -> defaultValue -}
fromTable :: (Muxable a, Select n, Default a) => Vec (P2 n) (Maybe a) -> Circuit (Vec n Wire) a
fromTable a = proc b -> select -< (fmap (maybe defaultValue id) a, b)
