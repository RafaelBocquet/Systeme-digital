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
import Magma.Vec
import Magma.Nat

type Registers     = Vec (N 32) (Vec (N 32) Wire)
type RegisterIndex = Vec (N 5) Wire
type V32 = Vec (N 32) Wire

registerFetch :: Circuit (Registers, RegisterIndex) V32
registerFetch = select

registerUnit :: (Registers -> Circuit a (Registers, b)) -> Circuit a b
registerUnit = registerLike

-- generate all register indexes -> Vec 32 Registers -> mux this
-- may actually be inefficient (32 * 32 * 5 muxes)
-- actually the only way to do it
updateRegister :: Circuit (Registers, RegisterIndex, V32) Registers
updateRegister = undefined
