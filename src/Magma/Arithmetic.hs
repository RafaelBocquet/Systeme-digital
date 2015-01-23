{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}

module Magma.Arithmetic where

import Prelude hiding (id, (.), and, or, zip)
import Data.Traversable
import Data.Foldable hiding (and, or)
import Data.Functor
import Data.Monoid
import Control.Applicative
import Control.Arrow
import Control.Category

import qualified GHC.TypeLits

import Magma.Circuit
import Magma.Nat
import Magma.Vec

halfAdder :: MonadCircuit m => Circuit m (Wire m, Wire m) (Wire m, Wire m)
halfAdder = xor2 &&& and2

fullAdder :: MonadCircuit m => Circuit m (Wire m, Wire m, Wire m) (Wire m, Wire m)
fullAdder = proc (a, b, c) -> do
  (s, r)     <- halfAdder -< (a, b)
  (s', r')   <- halfAdder -< (s, c)
  second or2 -< (s', (r, r'))

class ParallelAdder n where
  parallelAdder' :: MonadCircuit m => Circuit m ((Wire m), Vec n (Wire m), Vec n (Wire m)) (Vec n (Wire m), (Wire m))
instance ParallelAdder Z where
  parallelAdder' = proc (r, _, _) -> do
    returnA -< (VNil, r)
instance ParallelAdder n => ParallelAdder (S n) where
  parallelAdder' = proc (r, a :> as, b :> bs) -> do
    (c, r')   <- fullAdder      -< (r, a, b)
    (cs, r'') <- parallelAdder' -< (r', as, bs)
    returnA   -< (c `VCons` cs, r'')

-- | 'parallelAdder' is a 'Circuit' performing addition of two n-bit unsigned integers.
-- This circuit has a linear depth
parallelAdder :: (MonadCircuit m, ParallelAdder n) => Circuit m (Vec n (Wire m), Vec n (Wire m)) (Vec n (Wire m), (Wire m))
parallelAdder = proc (a, b) -> do
  parallelAdder' -< (constWire False, a, b)

parallelAdder32 :: MonadCircuit m => Circuit m (Vec (N 32) (Wire m), Vec (N 32) (Wire m)) (Vec (N 32) (Wire m), (Wire m))
parallelAdder32 = parallelAdder

-- | Logarithmic adder : won't be used in practice as it has more gates than the linear one and the simulator execution time is in O(number of gates)
class LogParallelAdder m n where 
  logParallelAdder' :: MonadCircuit m => SNat n -> Circuit m (Vec (P2 n) (Wire m), Vec (P2 n) (Wire m)) ((Vec (P2 n) (Wire m), (Wire m)), (Vec (P2 n) (Wire m), (Wire m)))
instance LogParallelAdder m Z where
  logParallelAdder' SZ = proc (a :> _, b :> _) -> do
    (s0, r0) <- fullAdder -< (constWire False, a, b)
    (s1, r1) <- fullAdder -< (constWire True, a, b)
    returnA  -< ((s0 `VCons` VNil, r0), (s1 `VCons` VNil, r1))
instance (NatSingleton (P2 n), Muxable m (Vec (P2 n) (Wire m)), LogParallelAdder m n) => LogParallelAdder m (S n) where
  logParallelAdder' (SS n) = proc (a :| a', b :| b') -> do
    ((s0,  r0),  (s1,  r1)) <- logParallelAdder' n -< (a, b)
    (sr0', sr1')            <- logParallelAdder' n -< (a', b')
    (s0', r0')              <- mux -< (r0, sr0', sr1')
    (s1', r1')              <- mux -< (r1, sr0', sr1')
    returnA -< ((s0 `vappend` s0', r0'), (s1 `vappend` s1', r1'))

-- | 'logParallelAdder32' performs the addition of two 32-bit unsigned integers.
-- This circuit has a logarithmic depth.
logParallelAdder32 :: MonadCircuit m => Circuit m (Vec (N 32) (Wire m), Vec (N 32) (Wire m)) ((Vec (N 32) (Wire m), (Wire m)), (Vec (N 32) (Wire m), (Wire m)))
logParallelAdder32 = logParallelAdder' (natSingleton :: SNat (N 5))


parallelSubs :: (MonadCircuit m, ParallelAdder n) => Circuit m (Vec n (Wire m), Vec n (Wire m)) (Vec n (Wire m),(Wire m))
parallelSubs = proc (a,b) -> do
  b' <- bitwise not1 -< b
  parallelAdder' -< (constWire True, a, b')

parallelAddSub :: (MonadCircuit m, GenerateVec n, ParallelAdder n) => Circuit m ((Wire m),Vec n (Wire m),Vec n (Wire m)) (Vec n (Wire m), (Wire m))
parallelAddSub = proc (control, a, b) -> do
  b' <- bitwise xor2 -< zip (replicateVec control) b
  parallelAdder' -< (control, a, b')

-- needs testing
class ParallelMult m i j where
  parallelMult :: MonadCircuit m => Circuit m (Vec (S i) (Wire m), Vec j (Wire m)) (Vec (j + i) (Wire m))
instance ParallelMult m Z j where
  parallelMult = proc (a :> _, b) -> do
    bitwise (proc x -> and2 -< (a, x)) -<< b
instance (NatSingleton i, NatSingleton j, GenerateVec i, ParallelAdder (j + i), ParallelMult m i j) => ParallelMult m(S i) j where
  parallelMult = proc (a :> as, b) -> do
    b'  <- bitwise (proc c -> and2 -< (a, c)) -<< b
    let alignB = b' `vappendComm` (replicateVec (constWire False) :: Vec i (Wire m))
    c <- parallelMult -< (as, b)
    arr fst . parallelAdder -< (constWire False `VCons` alignB, constWire False `VCons` c)

parallelMult32 :: MonadCircuit m => Circuit m (Vec (N 32) (Wire m), Vec (N 32) (Wire m)) (Vec (N 63) (Wire m))
parallelMult32 = parallelMult
