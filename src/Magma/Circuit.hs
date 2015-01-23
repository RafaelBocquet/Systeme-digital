{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase, RankNTypes, FlexibleContexts #-}
{-# OPTIONS_GHC -fcontext-stack=50 #-}

module Magma.Circuit where

import Prelude hiding (id, (.), and, or, zip, foldr1, concat,sequence)
import Data.Traversable
import Data.Foldable hiding (and, or)
import Data.Functor
import Data.Bifoldable
import Data.Monoid
import Data.Type.Equality
import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Functor.Identity
import Control.Monad.State hiding (sequence)
import Data.Proxy

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified GHC.TypeLits

import Magma.Nat
import Magma.Vec

class (Monad m, Eq (Wire m)) => MonadCircuit m where
  data Wire m
  constWire :: Bool -> Wire m
  viewConstWire :: Wire m -> Maybe Bool
  addEquation :: Equation m -> m (Wire m)
  reg :: (Wire m -> Circuit m a (Wire m, b)) -> Circuit m a b
  rom :: Circuit m (Vec (N 28) (Wire m)) (Vec (N 32) (Wire m))
  ram :: (Vec (N 32) (Wire m) -> Circuit m a (Vec (N 28) (Wire m), Vec (N 32) (Wire m), (Wire m), b)) -> Circuit m (Vec (N 28) (Wire m), a) b


pattern ConstW b <- (viewConstWire -> Just b)

data Equation m = EAnd (Wire m) (Wire m)
                | EOr (Wire m) (Wire m)
                | EXor (Wire m) (Wire m)
                | ENand (Wire m) (Wire m)
                | ENot (Wire m)
                | EMux (Wire m) (Wire m) (Wire m)
                | ERom (Vec (N 28) (Wire m)) -- ^ Read address
                | ERam (Vec (N 28) (Wire m)) (Vec (N 28) (Wire m)) (Vec (N 32) (Wire m)) (Wire m) -- ^ Read address | Write address | Write enable
                | ESelect (Wire m) Int -- ^ Only used internally
                | EReg (Wire m)

data CircuitState = CircuitState
                    { circuitFresh     :: Int
                    , circuitEquations :: Map Int (Equation CircuitMonad)
                    }

emptyCircuitState :: CircuitState
emptyCircuitState = CircuitState 0 Map.empty

newtype CircuitMonad a = CircuitMonad { unCircuitMonad :: State CircuitState a }
                         deriving (Functor, Applicative, Monad, MonadState CircuitState)

instance MonadCircuit CircuitMonad where
  data Wire CircuitMonad = WWire Int
                         | WConst Bool
                         | WInput String
                         deriving(Eq, Ord)
                                 
  addEquation e = do
    WWire w <- freshWire
    modify $ \s -> s { circuitEquations = Map.insert w e (circuitEquations s) }
    return (WWire w)

  constWire = WConst

  viewConstWire (WConst a) = Just a
  viewConstWire _          = Nothing

  reg f = Circuit . Kleisli $ \a -> do
    WWire w <- freshWire
    (w', b) <- runKleisli (unCircuit $ f (WWire w)) a
    modify $ \s -> s { circuitEquations = Map.insert w (EReg w') (circuitEquations s) }
    return b

  rom = Circuit . Kleisli $ \a -> do
    WWire w  <- freshWire
    modify $ \s -> s { circuitEquations = Map.insert w (ERom a) (circuitEquations s) }
    ws <- traverse
          (\i -> do
              WWire w' <- freshWire
              modify $ \s -> s { circuitEquations = Map.insert w' (ESelect (WWire w) i) (circuitEquations s) }
              return (WWire w')
          )
          indicesVec
    return ws

  ram f = Circuit . Kleisli $ \(ra, a) -> do
    WWire w  <- freshWire
    ws <- traverse
          (\i -> do
              w' <- freshWire
              return (i, w')
          )
          indicesVec
    (wa, wd, we, b) <- runKleisli (unCircuit $ f (fmap snd ws)) a
    modify $ \s -> s { circuitEquations = Map.insert w (ERam ra wa wd we) (circuitEquations s) }
    traverse
      (\(i, WWire w') -> do 
          modify $ \s -> s { circuitEquations = Map.insert w' (ESelect (WWire w) i) (circuitEquations s) }
      )
      ws
    return b

newtype Circuit m a b = Circuit { unCircuit :: Kleisli m a b }
                      deriving (Category, Arrow, ArrowApply)

-- class CircuitIO a where
--   circuitWires :: a -> [Wire]
-- instance CircuitIO () where
--   circuitWires () = []
-- instance CircuitIO Wire where
--   circuitWires w = [w]
-- instance CircuitIO w => CircuitIO (Vec n w) where
--   circuitWires = foldMap circuitWires
-- instance (CircuitIO a, CircuitIO b) => CircuitIO (a, b) where
--   circuitWires = bifoldMap circuitWires circuitWires

-- class CircuitIO a => CircuitInput a where
--   circuitInput  :: String -> a
-- instance CircuitInput () where
--   circuitInput _ = ()
-- instance CircuitInput Wire where
--   circuitInput s = WInput (s ++ "w")
-- instance (GenerateVec n, CircuitInput w) => CircuitInput (Vec n w) where
--   circuitInput s = fmap (\i -> circuitInput (s ++ "i" ++ show i)) $ indicesVec
-- instance (CircuitInput a, CircuitInput b) => CircuitInput (a, b) where
--   circuitInput s = (circuitInput (s ++ "a"), circuitInput (s ++ "b"))

-- -- TODO : circuit input / output
-- -- | 'runCircuit' produces a string describing the netlist generated by the circuit
-- runCircuit :: (CircuitInput a, CircuitIO b) => Circuit a b -> String
-- runCircuit (Circuit c) =
--   let isConstWire (WConst _) = True
--       isConstWire _          = False
--       input = circuitInput ""
--       (output, circuitEquations -> eqs) = flip runState emptyCircuitState . unCircuitMonad $ runKleisli c input
--       inputs     = filter (not.isConstWire) $ circuitWires input
--       outputs    = filter (not.isConstWire) $ circuitWires output
--       vars       = Map.keys eqs
--       varname x  = "v" ++ show x
--       commasep [] = ""
--       commasep xs = foldr1 (\a b -> a ++ ", " ++ b) xs
--       printWire (WWire a)  = varname a
--       printWire (WInput s) = "i" ++ s
--       printWire (WConst False) = "0"
--       printWire (WConst True)  = "1"
--       printEquation (EAnd a b)     = "AND " ++ printWire a ++ " " ++ printWire b
--       printEquation (EOr a b)      = "OR " ++ printWire a ++ " " ++ printWire b
--       printEquation (EXor a b)     = "XOR " ++ printWire a ++ " " ++ printWire b
--       printEquation (ENand a b)    = "NAND " ++ printWire a ++ " " ++ printWire b
--       printEquation (ENot a)       = "NOT " ++ printWire a
--       printEquation (EMux a b c)   = "MUX " ++ printWire a ++ " " ++ printWire b ++ " " ++ printWire c
--       -- need to concat 28 wires
-- --      printEquation (ERom a)       = "ROM " ++ printWire a
-- --      printEquation (ERam a b c d) = "RAM " ++ printWire a ++ " " ++ printWire b ++ " " ++ printWire c ++ " " ++ printWire d
--       printEquation (ESelect a b)  = "SELECT " ++ show b ++ " " ++ printWire a
--       printEquation (EReg a)       = "REG " ++ printWire a
--   in concat
--      [ "INPUT "
--      , commasep (printWire <$> inputs)
--      , "\nOUTPUT "
--      , commasep (printWire <$> outputs)
--      , "\nVAR "
--      , commasep (fmap varname vars ++ fmap printWire inputs ++ fmap printWire outputs)
--      , "\nIN\n"
--      , concat (fmap (\(w, e) -> varname w ++ " = " ++ printEquation e ++ "\n") (Map.toList eqs)) 
--      ]
     

freshWire :: CircuitMonad (Wire CircuitMonad)
freshWire = do
  i <- circuitFresh <$> get
  modify $ \s -> s { circuitFresh = i + 1 }
  return (WWire i)

-- Primitives
-- We have to use monadic code here

and2, or2, xor2, nand2 :: MonadCircuit m => Circuit m (Wire m, Wire m) (Wire m)
and2 = Circuit . Kleisli $ \case
  (ConstW True,a)    -> return a
  (a,ConstW True)    -> return a
  (_,ConstW False)   -> return $ constWire False
  (ConstW False,_)   -> return $ constWire False 
  (a, b) | a == b    -> return a
         | otherwise -> addEquation (EAnd a b)
or2 = Circuit . Kleisli $ \case
  (ConstW False,a)   -> return a
  (a,ConstW False)   -> return a
  (ConstW True, _)   -> return $ constWire True
  (_, ConstW True)   -> return $ constWire True
  (a, b) | a == b    -> return a
         | otherwise -> addEquation (EOr a b)
xor2 = Circuit . Kleisli $ \case
  (ConstW False,a)          -> return a
  (a,ConstW False)          -> return a
  (ConstW True,ConstW True) -> return $ constWire False
  (a, b) | a == b           -> return $ constWire False
         | otherwise        -> addEquation (EXor a b)
nand2 = Circuit . Kleisli $ \case 
  (ConstW True,ConstW True) -> return $ constWire False
  (ConstW False, _)         -> return $ constWire True
  (_,ConstW False)          -> return $ constWire True
  (a, b)                    -> addEquation (ENand a b)

not1 :: MonadCircuit m => Circuit m (Wire m) (Wire m)
not1 = Circuit . Kleisli $ \case
  (ConstW b) -> return $ constWire (not b)
  a          -> addEquation (ENot a)

mux3 :: MonadCircuit m => Circuit m (Wire m, Wire m, Wire m) (Wire m)
mux3 = Circuit . Kleisli $ \case
  (ConstW True, b, _)   -> return b
  (ConstW False, _, c)  -> return c  
  (a, b, c) | b == c    -> return b
            | otherwise -> addEquation (EMux a b c)

-- -- ...

class MonadCircuit m => RegisterLike m r where
  registerLike :: (r -> Circuit m a (r, b)) -> Circuit m a b
instance MonadCircuit m => RegisterLike m (Wire m) where
  registerLike = reg
instance MonadCircuit m => RegisterLike m (Vec Z r) where
  registerLike f = arr snd . f VNil
instance (RegisterLike m r, RegisterLike m (Vec n r)) => RegisterLike m (Vec (S n) r) where
  registerLike f = registerLike
                   $ \a -> registerLike
                           $ \as -> proc i -> do
                             (b :> bs, o) <- f (a `VCons` as) -< i
                             returnA -< (bs, (b, o))                        

class MonadCircuit m => Muxable m a where
  -- | 'mux' is circuit whose output is either its second or third input, depending on its first input.
  mux :: Circuit m (Wire m, a, a) a
instance MonadCircuit m => Muxable m (Wire m) where
  mux = mux3
instance (Muxable m a, Muxable m b) => Muxable m (a, b) where
  mux = proc (a, (b0, c0), (b1, c1)) -> do
    b <- mux -< (a, b0, b1)
    c <- mux -< (a, c0, c1)
    returnA -< (b, c)
instance Muxable m a => Muxable m (Vec Z a) where
  mux = proc _ -> returnA -< VNil
instance (Muxable m a, Muxable m (Vec n a)) => Muxable m (Vec (S n) a) where
  mux = proc (a, b :> bs, c :> cs) -> do
    d       <- mux -< (a, b, c)
    ds      <- mux -< (a, bs, cs)
    returnA -< d `VCons` ds

class MonadCircuit m => Select m n where
  -- | 'select' is a circuit that performs a lookup in a lookup table of size (2 ^ n) using a n-bit index.
  select :: Muxable m a => Circuit m (Vec (P2 n) a, Vec n (Wire m)) a
  -- May have found a GHC bug when pattern matching with (a :> VNil)
instance MonadCircuit m => Select m Z where
  select = proc (a :> _, _) -> returnA -< a
instance (NatSingleton (P2 n), Select m n) => Select m (S n) where
  select = proc (b :| c, a :> as) -> do
    b' <- select -< (b, as)
    c' <- select -< (c, as)
    mux -< (a, b', c')

class MonadCircuit m => Default m a where
  defaultValue :: Proxy m -> a
instance MonadCircuit m => Default m (Wire m) where
  defaultValue _ = constWire False
instance MonadCircuit m => Default m (Vec Z a) where
  defaultValue _ = VNil
instance (Default m a, Default m (Vec n a)) => Default m (Vec (S n) a) where
  defaultValue m = VCons (defaultValue m) (defaultValue m)

{- needs to be implemented from BDD -}
{- Maybe -> Nothing = dont care ? -}
{- Only DontCares -> defaultValue -}
fromTable :: forall m a n. (Muxable m a, Select m n, Default m a) => Vec (P2 n) (Maybe a) -> Circuit m (Vec n (Wire m)) a
fromTable a = proc b -> select -< (fmap (maybe (defaultValue (Proxy :: Proxy m)) id) a, b)

bitwise :: (MonadCircuit m, Traversable t) => Circuit m a b -> Circuit m (t a) (t b)
bitwise (runKleisli . unCircuit -> f) =
  Circuit . Kleisli $
  \x -> sequence (f <$> x)
