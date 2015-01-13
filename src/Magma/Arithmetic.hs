{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms #-}

module Magma.Arithmetic where

import Magma.Circuit

import Prelude hiding (id, (.), and, or, zip)
import Data.Traversable
import Data.Foldable hiding (and, or)
import Data.Functor
import Data.Monoid
import Control.Applicative
import Control.Arrow
import Control.Category

import qualified GHC.TypeLits

halfAdder :: Circuit (Wire, Wire) (Wire, Wire)
halfAdder = xor2 &&& and2

fullAdder :: Circuit (Wire, Wire, Wire) (Wire, Wire)
fullAdder = proc (a, b, c) -> do
  (s, r)     <- halfAdder -< (a, b)
  (s', r')   <- halfAdder -< (s, c)
  second or2 -< (s', (r, r'))

class SerialAdder n where
  serialAdder' :: Circuit (Wire, Vec n Wire, Vec n Wire) (Vec n Wire, Wire)
instance SerialAdder Z where
  serialAdder' = proc (r, _, _) -> do
    returnA -< (VNil, r)
instance SerialAdder n => SerialAdder (S n) where
  serialAdder' = proc (r, a :> as, b :> bs) -> do
    (c, r')   <- fullAdder    -< (r, a, b)
    (cs, r'') <- serialAdder' -< (r', as, bs)
    returnA   -< (c `VCons` cs, r'')

serialAdder :: SerialAdder n => Circuit (Vec n Wire, Vec n Wire) (Vec n Wire, Wire)
serialAdder = proc (a, b) -> do
  r            <- const0 -< False
  serialAdder' -< (r, a, b)

