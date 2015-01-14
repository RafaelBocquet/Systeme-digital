{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase #-}

module Magma.Vec where

import Prelude hiding (id, (.), and, or, zip, foldr)
import Data.Traversable
import Data.Foldable hiding (and, or)
import Data.Functor
import Data.Monoid
import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Functor.Identity
import Control.Monad.State

import qualified GHC.TypeLits

import Magma.Nat

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Zippable
class Zippable f where
  zip :: f a -> f b -> f (a, b)

-- Vec
data Vec n a where
  VNil  :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

viewVCons :: Vec (S n) a -> (a, Vec n a)
viewVCons (VCons a as) = (a, as)
pattern a :> as <- (viewVCons -> (a, as))

class GenerateVec n where
  generateVec :: Applicative f => f a -> f (Vec n a)
instance GenerateVec Z where
  generateVec f = pure VNil
instance GenerateVec n => GenerateVec (S n) where
  generateVec f = VCons <$> f <*> generateVec f

emptyVec :: GenerateVec n => Vec n ()
emptyVec = runIdentity (generateVec (Identity ()))

toNameVec :: GenerateVec n => Vec n Int
toNameVec = flip evalState 0 . generateVec $ do
  i <- get
  modify (+ 1)
  return i

splitVec :: NatSingleton n => Vec (m + n) a -> (Vec n a, Vec m a)
splitVec = withNatSingleton splitVec'

splitVec' :: SNat n -> Vec (m + n) a -> (Vec n a, Vec m a)
splitVec' SZ     v            = (VNil, v)
splitVec' (SS n) (VCons a as) = let (b, c) = splitVec' n as
                                in (VCons a b, c)

-- Split a vector into two vectors
pattern a :| b <- (splitVec -> (a, b))
-- Split a vector into two vectors with matching sizes
-- pattern (:|:) :: () => NatSingleton n => Vec n a -> Vec n a -> Vec (n + n) a
-- pattern a :|: b <- (splitVec -> (a, b))

vappend :: NatSingleton n => Vec n a -> Vec m a -> Vec (m + n) a
vappend = withNatSingleton vappend'

vappend' :: SNat n -> Vec n a -> Vec m a -> Vec (m + n) a
vappend' SZ     VNil         v = v
vappend' (SS n) (VCons a as) v = a `VCons` vappend' n as v

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

fromList :: ExpQ -> ExpQ
fromList = flip (>>=) $ \case
  ListE l -> foldr (\a b -> appE (appE (conE 'VCons) (return a)) b) (conE 'VNil) l
  _       -> fail $ "fromList expects a list"
