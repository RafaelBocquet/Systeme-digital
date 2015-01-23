{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms #-}

module Magma.CPU where

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
import Magma.Instruction
import Magma.ALU

-- cpu :: MonadCircuit m => Circuit m () ()
-- cpu =
--   registerLike $ \(regs, pc, hilo) -> proc () -> do
--     i         <- readInstruction regs . rom -< pc
--     aluInput  <- instructionAluInput hilo   -< i
--     aluOutput <- alu                        -< aluInput
--     returnA -< ((_, _, aluOMUL aluOutput), ())
