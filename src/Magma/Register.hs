{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms #-}

module Magma.Register where

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

type Registers     = Vec (N 32) (Vec (N 32) Wire)
type RegisterIndex = Vec (N 5) Wire
type V32 = Vec (N 32) Wire

registerFetch :: Circuit (Registers, RegisterIndex) V32
registerFetch = select

-- registerUnit :: ([Nappe 5] -> Circuit a ([Nappe 5], b)) -> Circuit a ([Nappe 5], b)
-- registerUnit = undefined

-- generate all register indexes -> Vec 32 Registers -> mux this
-- may seem to be inefficient, but is not
updateRegister :: Circuit (Registers, RegisterIndex, V32) Registers
updateRegister = undefined
