{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, ScopedTypeVariables #-}

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
import Magma.Arithmetic
import Magma.Register

cpu :: forall m. MonadCircuit m => Circuit m () ()
cpu =
  registerLike $ \(regs, pc, hilo) -> proc () -> do
    i         <- readInstruction regs . rom -< pc
    aluInput  <- instructionAluInput hilo   -< i
    aluOutput <- alu                        -< aluInput
    newPc     <- do
      aluPc  <- instructionAluPc -< i
      incrPc <- increment        -< pc
      let (_ :: Vec (N 2) (Wire m)) :| aluOPc :| _ = aluO aluOutput
      mux    -< (aluPc, aluOPc, incrPc)
    regWe     <- instructionRegisterWriteEnable          -< i
    regIx     <- instructionDestinationRegister          -< i
    regDt     <- instructionRegisterData newPc aluOutput -<< i
    regs'     <- updateRegister                          -< (regWe, regs, regIx, undefined)
    returnA   -< ((regs, newPc, aluOMUL aluOutput), ())
