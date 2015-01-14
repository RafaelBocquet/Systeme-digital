{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, RankNTypes, FlexibleContexts #-}

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

halfAdder :: Circuit (Wire, Wire) (Wire, Wire)
halfAdder = xor2 &&& and2

fullAdder :: Circuit (Wire, Wire, Wire) (Wire, Wire)
fullAdder = proc (a, b, c) -> do
  (s, r)     <- halfAdder -< (a, b)
  (s', r')   <- halfAdder -< (s, c)
  second or2 -< (s', (r, r'))

class ParallelAdder n where
  parallelAdder' :: Circuit (Wire, Vec n Wire, Vec n Wire) (Vec n Wire, Wire)
instance ParallelAdder Z where
  parallelAdder' = proc (r, _, _) -> do
    returnA -< (VNil, r)
instance ParallelAdder n => ParallelAdder (S n) where
  parallelAdder' = proc (r, a :> as, b :> bs) -> do
    (c, r')   <- fullAdder      -< (r, a, b)
    (cs, r'') <- parallelAdder' -< (r', as, bs)
    returnA   -< (c `VCons` cs, r'')

parallelAdder :: ParallelAdder n => Circuit (Vec n Wire, Vec n Wire) (Vec n Wire, Wire)
parallelAdder = proc (a, b) -> do
  parallelAdder' -< (WConst False, a, b)

-- It doesn't seem possible not to use SNat...
-- It would be possible once ghc#6018 (Injective type families) is implemented
class LogParallelAdder n where
  logParallelAdder' :: SNat n -> Circuit (Vec (P2 n) Wire, Vec (P2 n) Wire) ((Vec (P2 n) Wire, Wire), (Vec (P2 n) Wire, Wire))
instance LogParallelAdder Z where
  logParallelAdder' SZ = proc (a :> _, b :> _) -> do
    (s0, r0) <- fullAdder -< (WConst False, a, b)
    (s1, r1) <- fullAdder -< (WConst True, a, b)
    returnA  -< ((s0 `VCons` VNil, r0), (s1 `VCons` VNil, r1))
instance (NatSingleton (P2 n), Muxable (Vec (P2 n) Wire), LogParallelAdder n) => LogParallelAdder (S n) where
  logParallelAdder' (SS n) = proc (a :| a', b :| b') -> do
    ((s0,  r0),  (s1,  r1)) <- logParallelAdder' n -< (a, b)
    (sr0', sr1')            <- logParallelAdder' n -< (a', b')
    (s0', r0')              <- mux -< (r0, sr0', sr1')
    (s1', r1')              <- mux -< (r1, sr0', sr1')
    returnA -< ((s0 `vappend` s0', r0'), (s1 `vappend` s1', r1'))

logParallelAdder32 :: Circuit (Vec (N 32) Wire, Vec (N 32) Wire) ((Vec (N 32) Wire, Wire), (Vec (N 32) Wire, Wire))
logParallelAdder32 = logParallelAdder' (natSingleton :: SNat (N 5))
