{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, ViewPatterns, PatternSynonyms #-}

module Magma.Instruction where

import Magma.Circuit
import Magma.Register

import Prelude hiding (id, (.), and, or, zip)
import Data.Traversable
import Data.Foldable hiding (and, or)
import Data.Functor
import Data.Monoid
import Control.Applicative
import Control.Arrow
import Control.Category

import qualified GHC.TypeLits

data Instruction = Instruction
                   { instructionOpcode    :: Vec (N 6) Wire
                   , instructionRs        :: RegisterIndex
                   , instructionRt        :: RegisterIndex
                   , instructionRd        :: RegisterIndex
                   , instructionShamt     :: Vec (N 5) Wire
                   , instructionFunct     :: Vec (N 6) Wire
                   , instructionImmediate :: Vec (N 16) Wire
                   , instructionAddress   :: Vec (N 26) Wire
                   }

readInstruction :: Circuit V32 Instruction
readInstruction = proc (o :| i) -> do
  let rs :| rt :| i'       = i
  let rd :| shamt :| funct = i'
  returnA -< Instruction o rs rt rd shamt funct i' i
