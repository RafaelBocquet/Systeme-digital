{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase, RankNTypes,TemplateHaskell #-}

module Magma.Vec where

import Prelude hiding (id, (.), and, or, zip, foldr)
import Data.Traversable
import Data.Foldable hiding (and, or)
import Data.Functor
import Data.Monoid
import Data.Type.Equality
import Control.Applicative
import Control.Monad
import Control.Category
import Control.Monad.Identity
import Control.Monad.State hiding (lift)

import qualified GHC.TypeLits

import Magma.Nat

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Zippable
class Zippable f where
  zip :: f a -> f b -> f (a, b)

-- | 'Vec' n is the type of lists of length n
data Vec n a where
  VNil  :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

infixr 9 `VCons`

viewVCons :: Vec (S n) a -> (a, Vec n a)
viewVCons (VCons a as) = (a, as)
pattern a :> as <- (viewVCons -> (a, as))

class GenerateVec n where
  -- | 'generateVec' creates a vector of length n with an applicative action
  -- 
  -- see 'replicateVec' or 'indicesVec' for examples
  generateVec :: Applicative f => f a -> f (Vec n a)
instance GenerateVec Z where
  generateVec f = pure VNil
instance GenerateVec n => GenerateVec (S n) where
  generateVec f = VCons <$> f <*> generateVec f

-- | 'replicateVec' a is the vector of length n with only a as elements
replicateVec :: GenerateVec n => a -> Vec n a
replicateVec = runIdentity . generateVec . Identity

-- | 'indicesVec' is the vector of length n whose i-th element is i
indicesVec :: GenerateVec n => Vec n Int
indicesVec = flip evalState 0 . generateVec $ do
  i <- get
  modify (+ 1)
  return i

-- |Split a vector into two vector
splitVec :: NatSingleton n => Vec (m + n) a -> (Vec n a, Vec m a)
splitVec = withNatSingleton splitVec'

-- |Explicit 'splitVec'
splitVec' :: SNat n -> Vec (m + n) a -> (Vec n a, Vec m a)
splitVec' SZ     v            = (VNil, v)
splitVec' (SS n) (VCons a as) = let (b, c) = splitVec' n as
                                in (VCons a b, c)

-- |Pattern synonym to split a vector into two vectors
pattern a :| b <- (splitVec -> (a, b))

-- |Vector concatenation
vappend :: NatSingleton n => Vec n a -> Vec m a -> Vec (m + n) a
vappend = withNatSingleton vappend'

-- |Explicit 'vappend'
vappend' :: SNat n -> Vec n a -> Vec m a -> Vec (m + n) a
vappend' SZ     VNil         v = v
vappend' (SS n) (VCons a as) v = a `VCons` vappend' n as v

-- |Vector concatenation (using commutativity of plus)
vappendComm :: forall a n m. (NatSingleton n, NatSingleton m) => Vec n a -> Vec m a -> Vec (n + m) a
vappendComm a b = case plusComm (natSingleton :: SNat n) (natSingleton :: SNat m) of
  Refl -> vappend a b

deriving instance Show a => Show (Vec n a)
instance Functor (Vec n) where
  fmap f VNil         = VNil
  fmap f (VCons a as) = f a `VCons` fmap f as
instance Foldable (Vec n) where
  foldMap f VNil         = mempty
  foldMap f (VCons a as) = f a <> foldMap f as
instance Traversable (Vec n) where
  traverse f VNil         = pure VNil
  traverse f (VCons a as) = VCons <$> f a <*> traverse f as
instance Zippable (Vec n) where
  zip VNil VNil                 = VNil
  zip (VCons a as) (VCons b bs) = (a, b) `VCons` zip as bs

-- |Template Haskell function to define inline vectors with list syntax
-- @
--   $(fromList [| [a, b, c] |]) == a `VCons` b `VCons` c `VCons` VNil
-- @
fromList :: ExpQ -> ExpQ
fromList = flip (>>=) $ \case
  ListE l -> foldr (\a b -> appE (appE (conE 'VCons) (return a)) b) (conE 'VNil) l
  _       -> fail $ "fromList expects a list"

-- |Like 'Vec', but the types of its elements are indexed by their index in the vector
data IVec n a where
  IVNil  :: IVec Z a
  IVCons :: a n -> IVec n a -> IVec (S n) a

class GenerateIVec n where
  -- |Like 'generateVec' for 'IVec'
  generateIVec :: Applicative f => (forall i. f (a i)) -> f (IVec n a)
instance GenerateIVec Z where
  generateIVec f = pure IVNil
instance GenerateIVec n => GenerateIVec (S n) where
  generateIVec f = IVCons <$> f <*> generateIVec f
