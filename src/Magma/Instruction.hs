{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, TypeOperators, TypeFamilies, ViewPatterns, PatternSynonyms #-}

module Magma.Instruction where


import Prelude hiding (id, (.), and, or, zip, elem)
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
            deriving (Eq, Ord)

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
           | FnSyscall
           | FnUnknown
           deriving(Eq, Ord)

functTable :: Vec (N 64) Funct
functTable = $(fromList [|
                         [ FnSll,     FnUnknown, FnSlr,     FnSra,     FnSllv,    FnSrlv,    FnSrav,    FnUnknown -- 00
                         , FnJr,      FnUnknown, FnUnknown, FnUnknown, FnSyscall, FnUnknown, FnUnknown, FnUnknown -- 08
                         , FnMfhi,    FnUnknown, FnMflo,    FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 10
                         , FnMul,     FnMulu,    FnDiv,     FnDivu,    FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 18
                         , FnAdd,     FnAddu,    FnSub,     FnSubu,    FnAnd,     FnOr,      FnXor,     FnNor     -- 20
                         , FnUnknown, FnUnknown, FnSlt,     FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 28
                         , FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 30
                         , FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown, FnUnknown -- 38
                         ] |])

opcodeRegisterWriteEnable :: Wire -> Opcode -> Maybe Wire
opcodeRegisterWriteEnable f OpArith       = Just f
opcodeRegisterWriteEnable f OpUnknown     = Nothing
opcodeRegisterWriteEnable f op
  | op `elem` [OpJ, OpBeq, OpBne, OpSw]   = Just (WConst False)
  | op `elem` [OpAddi, OpAddiu, OpAndi,
               OpOri, OpLui, OpLw, OpJal] = Just (WConst True)

functRegisterWriteEnable :: Funct -> Maybe Wire
functRegisterWriteEnable FnUnknown                              = Nothing
functRegisterWriteEnable fn
  | fn `elem` [FnMul, FnMulu, FnDiv, FnDivu, FnJr]              = Just (WConst False)
  | fn `elem` [FnAdd, FnAddu, FnSub, FnSubu, FnMfhi, FnMflo,
               FnAnd, FnOr, FnXor, FnNor, FnSlt, FnSll,
               FnSlr, FnSra, FnSllv, FnSrlv, FnSrav, FnSyscall] = Just (WConst False)

opcodeDestinationRegister :: RegisterIndex -> RegisterIndex -> Opcode -> Maybe RegisterIndex
opcodeDestinationRegister f rd OpArith                        = Just f
opcodeDestinationRegister f rd OpUnknown                      = Nothing
opcodeDestinationRegister f rd op
  | op `elem` [OpJ, OpJal, OpBeq, OpBne, OpSw]                = Nothing
  | op `elem` [OpAddi, OpAddiu, OpAndi, OpOri, OpLui, OpLw]   = Just rd
  | op `elem` [OpJal]                                         = Just $(registerIndex 31)

functDestinationRegister :: RegisterIndex -> Funct -> Maybe RegisterIndex
functDestinationRegister rd FnUnknown                = Nothing
functDestinationRegister rd FnSyscall                = Just $(registerIndex 2) -- $v0
functDestinationRegister rd fn
  | fn `elem` [FnMul, FnMulu, FnDiv, FnDivu, FnJr]   = Nothing
  | fn `elem` [FnAdd, FnAddu, FnSub, FnSubu, FnMfhi, FnMflo,
               FnAnd, FnOr, FnXor, FnNor, FnSlt, FnSll,
               FnSlr, FnSra, FnSllv, FnSrlv, FnSrav] = Just rd

opcodeMemoryWriteEnable :: Opcode -> Maybe Wire
opcodeMemoryWriteEnable OpSw      = Just (WConst True)
opcodeMemoryWriteEnable OpUnknown = Nothing
opcodeMemoryWriteEnable _         = Just (WConst False)

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
  fWe <- fromTable (functRegisterWriteEnable <$> functTable) -< instructionFunct i
  fromTable (opcodeRegisterWriteEnable fWe <$> opcodeTable) -<< instructionOpcode i
  
instructionDestinationRegister :: Circuit Instruction RegisterIndex
instructionDestinationRegister = proc i -> do
  fRe <- fromTable (functDestinationRegister (instructionRd i) <$> functTable) -<< instructionFunct i
  fromTable (opcodeDestinationRegister fRe (instructionRd i) <$> opcodeTable) -<< instructionOpcode i

instructionMemoryWriteEnable :: Circuit Instruction Wire
instructionMemoryWriteEnable = proc i -> do
  fromTable (opcodeMemoryWriteEnable <$> opcodeTable) -< instructionOpcode i
