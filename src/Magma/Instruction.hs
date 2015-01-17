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

data Funct = FnAdd
           | FnAddu
           | FnSub
           | FnSubu
           | FnMul
           | FnMulu
           | FnDiv
           | FnDivu
           | FnMfhi
           | FnMflo
           | FnAnd
           | FnOr
           | FnXor
           | FnNor
           | FnSlt
           | FnSll
           | FnSlr
           | FnSra
           | FnSllv
           | FnSrlv
           | FnSrav
           | FnJr
           | FnUnknown

-- done up to mflo

functTable :: Vec (N 64) Funct
functTable = $(fromList [|
                         [ FnSll,     FnUnknown, FnSlr,     FnSra,     FnSllv,    FnSrlv,    FnSrav,    FnUnknown -- 00
                         , FnJr,      FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 08
                         , FnMfhi,    FnUnknown, FnMflo,    FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 10
                         , FnMul,     FnMulu,    FnDiv,     FnDivu,    FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 18
                         , FnAdd,     FnAddu,    FnSub,     FnSubu,    FnAnd,     FnOr,      FnXor,     FnNor     -- 20
                         , FnUnknown, FnUnknown, FnSlt,     FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 28
                         , FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 30
                         , FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 38
                         ] |])

opcodeRegisterWriteEnable :: Wire -> Opcode -> Maybe Wire
opcodeRegisterWriteEnable f OpArith   = Just f
opcodeRegisterWriteEnable f OpJ       = Just (WConst False)
opcodeRegisterWriteEnable f OpJal     = Just (WConst False)
opcodeRegisterWriteEnable f OpBeq     = Just (WConst False)
opcodeRegisterWriteEnable f OpBne     = Just (WConst False)
opcodeRegisterWriteEnable f OpAddi    = Just (WConst True)
opcodeRegisterWriteEnable f OpAddiu   = Just (WConst True)
opcodeRegisterWriteEnable f OpAndi    = Just (WConst True)
opcodeRegisterWriteEnable f OpOri     = Just (WConst True)
opcodeRegisterWriteEnable f OpLui     = Just (WConst True)
opcodeRegisterWriteEnable f OpLw      = Just (WConst True)
opcodeRegisterWriteEnable f OpSw      = Just (WConst False)
opcodeRegisterWriteEnable f OpUnknown = Nothing

functRegisterWriteEnable :: Funct -> Maybe Wire
functRegisterWriteEnable = undefined

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

instructionRegisterWriteEnable :: Circuit Instruction Wire
instructionRegisterWriteEnable = proc i -> do
  fWe <- fromTable (functRegisterWriteEnable <$> functTable) -< (instructionFunct i)
  fromTable (opcodeRegisterWriteEnable fWe <$> opcodeTable) -<< (instructionOpcode i)
  
