{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, MultiParamTypeClasses, ScopedTypeVariables #-}

module Magma.Nat where

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

withNatSingleton :: NatSingleton n => (SNat n -> a) -> a
withNatSingleton f = f natSingleton

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
