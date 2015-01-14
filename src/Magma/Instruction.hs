{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, ViewPatterns, PatternSynonyms #-}

module Magma.Instruction where


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
import Magma.Register
import Magma.Vec
import Magma.Nat

data Opcode = OpArith
            | OpAddi
            | OpAddiu

            | OpLw
            | OpSw
            | OpLui
              
            | OpAndi
            | OpOri
              
            | OpBeq
            | OpBne

            | OpJ
            | OpJal

            | OpUnknown

opcodeTable :: Vec (N 64) Opcode
opcodeTable = $(fromList [|
                          [ OpArith,   OpUnknown, OpJ,       OpJal,     OpBeq,     OpBne,     OpUnknown, OpUnknown -- 00
                          , OpAddi,    OpAddiu,   OpUnknown, OpUnknown, OpAndi,    OpOri,     OpUnknown, OpLui     -- 08
                          , OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown -- 10
                          , OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown -- 18
                          , OpUnknown, OpUnknown, OpUnknown, OpLw,      OpUnknown, OpUnknown, OpUnknown, OpUnknown -- 20
                          , OpUnknown, OpUnknown, OpUnknown, OpSw,      OpUnknown, OpUnknown, OpUnknown, OpUnknown -- 28
                          , OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown -- 30
                          , OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown, OpUnknown -- 38
                          ] |])

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
