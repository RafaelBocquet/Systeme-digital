{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, MultiParamTypeClasses, ScopedTypeVariables #-}

module Magma.Nat where

import Data.Type.Equality

import qualified GHC.TypeLits

-- |Kind of type level naturals
-- Dependent functions can be defined in two ways :
--
-- * Implicit form : Using a type class A n with instances A Z and A n => A (S n)
-- @
--   class A n where { a :: A n }
--   instance A Z where { a = az }
--   instance A n => A (S n) where { a = as }
-- @
-- * Explicit form : Using a function with type 'SNat' n -> A n
-- @
--   f :: SNat n -> A n
--   f SZ = az
--   f ()
-- @
-- The explicit form is needed when n can't be inferred (e.g. when n only appears in non injective type synonyms)
data Nat = Z | S Nat
-- |Singleton type for 'Nat'
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

-- |'NatSingleton' provides 
class NatSingleton n where
  natSingleton :: SNat n
instance NatSingleton Z where
  natSingleton = SZ
instance NatSingleton n => NatSingleton (S n) where
  natSingleton = SS natSingleton

-- |'withNatSingleton' transforms an explicit function into an implicit one
withNatSingleton :: NatSingleton n => (SNat n -> a) -> a
withNatSingleton f = f natSingleton

-- |'Nat' literals
type family (N n) where
  N 0       = Z
  N n = S (N (n GHC.TypeLits.- 1))

-- |Type level sum
type family n + m where
  n + Z   = n
  n + S m = S (n + m)

leftS :: SNat n -> SNat m -> S n + m :~: S (n + m)
leftS n SZ = Refl
leftS n (SS m) = case leftS n m of
  Refl -> Refl

-- | Proof that the additition is commutative
plusComm :: SNat n -> SNat m -> (n + m) :~: (m + n)
plusComm SZ SZ     = Refl
plusComm SZ (SS m) = case plusComm SZ m of
  Refl -> Refl
plusComm (SS n) m  = case plusComm n m of
  Refl -> case leftS n m of
           Refl -> Refl

-- |Type level multiplication
type family n * m where
  n * Z   = Z
  n * S m = (n * m) + n

-- |Type level power of two
type family P2 n where
  P2 Z     = S Z
  P2 (S n) = P2 n + P2 n
