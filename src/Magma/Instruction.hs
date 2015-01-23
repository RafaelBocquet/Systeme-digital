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
import Magma.ALU


-- |Opcodes of MIPS instruction we handle
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

-- |Used to generate lookup tables from the opcode to data needed by the circuit
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

-- |Funct codes of MIPS function instructions we handle
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

-- |Used to generate lookup tables for function instructions
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

-- |Which opcodes need to write to a register
opcodeRegisterWriteEnable :: MonadCircuit m => (Wire m) -> Opcode -> Maybe (Wire m)
opcodeRegisterWriteEnable f OpArith       = Just f
opcodeRegisterWriteEnable f OpUnknown     = Nothing
opcodeRegisterWriteEnable f op
  | op `elem` [OpJ, OpBeq, OpBne, OpSw]   = Just (constWire False)
  | op `elem` [OpAddi, OpAddiu, OpAndi,
               OpOri, OpLui, OpLw, OpJal] = Just (constWire True)

-- |Which function instructions need to write to a register
functRegisterWriteEnable :: MonadCircuit m => Funct -> Maybe (Wire m)
functRegisterWriteEnable FnUnknown                              = Nothing
functRegisterWriteEnable fn
  | fn `elem` [FnMul, FnMulu, FnDiv, FnDivu, FnJr]              = Just (constWire False)
  | fn `elem` [FnAdd, FnAddu, FnSub, FnSubu, FnMfhi, FnMflo,
               FnAnd, FnOr, FnXor, FnNor, FnSlt, FnSll,
               FnSlr, FnSra, FnSllv, FnSrlv, FnSrav, FnSyscall] = Just (constWire False)

-- |What is the destination register of an opcode
opcodeDestinationRegister :: MonadCircuit m => RegisterIndex m -> RegisterIndex m -> Opcode -> Maybe (RegisterIndex m)
opcodeDestinationRegister f rd OpArith                        = Just f
opcodeDestinationRegister f rd OpUnknown                      = Nothing
opcodeDestinationRegister f rd op
  | op `elem` [OpJ, OpJal, OpBeq, OpBne, OpSw]                = Nothing
  | op `elem` [OpAddi, OpAddiu, OpAndi, OpOri, OpLui, OpLw]   = Just rd
  | op `elem` [OpJal]                                         = Just (constWire <$> $(registerIndex 31)) -- ra

-- |What is the destination register of a function instruction
functDestinationRegister :: MonadCircuit m => RegisterIndex m -> Funct -> Maybe (RegisterIndex m)
functDestinationRegister rd FnUnknown                = Nothing
functDestinationRegister rd FnSyscall                = Just (constWire <$> $(registerIndex 2)) -- v0
functDestinationRegister rd fn
  | fn `elem` [FnMul, FnMulu, FnDiv, FnDivu, FnJr]   = Nothing
  | fn `elem` [FnAdd, FnAddu, FnSub, FnSubu, FnMfhi, FnMflo,
               FnAnd, FnOr, FnXor, FnNor, FnSlt, FnSll,
               FnSlr, FnSra, FnSllv, FnSrlv, FnSrav] = Just rd

-- |Which opcodes need to write to memory
opcodeMemoryWriteEnable :: MonadCircuit m => Opcode -> Maybe (Wire m)
opcodeMemoryWriteEnable OpSw      = Just (constWire True)
opcodeMemoryWriteEnable OpUnknown = Nothing
opcodeMemoryWriteEnable _         = Just (constWire False)

opcodeAluOperation :: MonadCircuit m => AluOperation m -> Opcode -> Maybe (AluOperation m)
opcodeAluOperation f OpArith                                = Just f
opcodeAluOperation f OpUnknown                              = Nothing
opcodeAluOperation f op
  | op `elem` [OpJ, OpJal, OpBeq, OpBne, OpSw, OpLui, OpLw] = Nothing
  | op `elem` [OpAddi, OpAddiu]                             = Just aluAddOperation
  | op `elem` [OpAndi, OpOri]                               = Just aluOrOperation
  | op `elem` [OpJal]                                       = undefined -- TODO : add from PC ?

functAluOperation :: MonadCircuit m => Funct -> Maybe (AluOperation m)
functAluOperation FnUnknown                              = Nothing
functAluOperation fn
  | fn `elem` [FnSyscall, FnJr]                             = Nothing
  | fn `elem` [FnMul, FnMulu]                               = Just aluMulOperation
  | fn `elem` [FnDiv, FnDivu]                               = undefined -- TODO
  | fn `elem` [FnAdd, FnAddu, FnSub, FnSubu, FnSlt]         = Just aluAddOperation
  | fn `elem` [FnMfhi, FnMflo]                              = Nothing
  | fn `elem` [FnAnd, FnOr, FnNor]                          = Just aluOrOperation
  | fn `elem` [FnXor]                                       = Just aluXorOperation
  | fn `elem` [FnSll, FnSlr, FnSra, FnSllv, FnSrlv, FnSrav] = undefined -- TODO

opcodeAluInput1 :: MonadCircuit m => V32 m -> Instruction m -> Opcode -> Maybe (V32 m)
opcodeAluInput1 f i OpArith                                 = Just f
opcodeAluInput1 f i OpUnknown                               = Nothing
opcodeAluInput1 f i op
  | op `elem` [OpJ, OpJal, OpBeq, OpBne, OpSw, OpLui, OpLw] = Nothing
  | op `elem` [OpAddi, OpAddiu, OpAndi, OpOri]              = Just $ instructionRsValue i
  | op `elem` [OpJal]                                       = undefined -- TODO : add from PC ?

functAluInput1 :: MonadCircuit m => Instruction m -> Funct -> Maybe (V32 m)
functAluInput1 i FnUnknown                                    = Nothing
functAluInput1 i fn
  | fn `elem` [FnSyscall, FnJr]                             = Nothing
  | fn `elem` [FnMul, FnMulu, FnAdd, FnAddu, FnSub,
               FnSubu, FnSlt, FnAnd, FnOr, FnXor, FnNor]    = Just $ instructionRsValue i
  | fn `elem` [FnMfhi, FnMflo]                              = Nothing
  | fn `elem` [FnDiv, FnDivu]                               = undefined -- TODO
  | fn `elem` [FnSll, FnSlr, FnSra, FnSllv, FnSrlv, FnSrav] = undefined -- TODO

opcodeAluInput2 :: MonadCircuit m => V32 m -> Instruction m -> Opcode -> Maybe (V32 m)
opcodeAluInput2 f i OpArith                                   = Just f
opcodeAluInput2 f i OpUnknown                                 = Nothing
opcodeAluInput2 f i op
  | op `elem` [OpJ, OpJal, OpBeq, OpBne, OpSw, OpLui, OpLw] = Nothing
  | op `elem` [OpAddi, OpAddiu, OpAndi, OpOri]              = Just $ (replicateVec (constWire False) :: MonadCircuit m => Vec (N 16) (Wire m)) `vappend` instructionImmediate i
  | op `elem` [OpJal]                                       = undefined -- TODO : add from PC ?

functAluInput2 :: MonadCircuit m => Instruction m -> Funct -> Maybe (V32 m)
functAluInput2 i FnUnknown                                    = Nothing
functAluInput2 i fn
  | fn `elem` [FnSyscall, FnJr]                             = Nothing
  | fn `elem` [FnMul, FnMulu, FnAdd, FnAddu, FnSub,
               FnSubu, FnSlt, FnAnd, FnOr, FnXor, FnNor]    = Just $ instructionRtValue i
  | fn `elem` [FnMfhi, FnMflo]                              = Nothing
  | fn `elem` [FnDiv, FnDivu]                               = undefined -- TODO
  | fn `elem` [FnSll, FnSlr, FnSra, FnSllv, FnSrlv, FnSrav] = undefined -- TODO

opcodeAluNegateInput1 :: MonadCircuit m => (Wire m) -> Opcode -> Maybe (Wire m)
opcodeAluNegateInput1 f OpArith                             = Just f
opcodeAluNegateInput1 f OpUnknown                           = Nothing
opcodeAluNegateInput1 f op
  | op `elem` [OpJ, OpJal, OpBeq, OpBne, OpSw, OpLui, OpLw] = Nothing
  | op `elem` [OpAddi, OpAddiu, OpAndi, OpOri]              = Just $ constWire False
  | op `elem` [OpJal]                                       = undefined -- TODO : add from PC ?

functAluNegateInput1 :: MonadCircuit m => Funct -> Maybe (Wire m)
functAluNegateInput1 FnUnknown                              = Nothing
functAluNegateInput1 fn
  | fn `elem` [FnSyscall, FnJr]                             = Nothing
  | fn `elem` [FnMul, FnMulu, FnAdd, FnAddu,
               FnSlt, FnOr, FnNor, FnXor, FnSub, FnSubu]    = Just $ constWire False
  | fn `elem` [FnAnd]                                       = Just $ constWire True
  | fn `elem` [FnMfhi, FnMflo]                              = Nothing
  | fn `elem` [FnDiv, FnDivu]                               = undefined -- TODO
  | fn `elem` [FnSll, FnSlr, FnSra, FnSllv, FnSrlv, FnSrav] = undefined -- TODO

opcodeAluNegateInput2 :: MonadCircuit m => (Wire m) -> Opcode -> Maybe (Wire m)
opcodeAluNegateInput2 f OpArith                             = Just f
opcodeAluNegateInput2 f OpUnknown                           = Nothing
opcodeAluNegateInput2 f op
  | op `elem` [OpJ, OpJal, OpBeq, OpBne, OpSw, OpLui, OpLw] = Nothing
  | op `elem` [OpAddi, OpAddiu, OpOri]                      = Just $ constWire False
  | op `elem` [OpAndi]                                    = Just $ constWire True
  | op `elem` [OpJal]                                       = undefined -- TODO : add from PC ?

functAluNegateInput2 :: MonadCircuit m => Funct -> Maybe (Wire m)
functAluNegateInput2 FnUnknown                            = Nothing
functAluNegateInput2 fn
  | fn `elem` [FnSyscall, FnJr]                             = Nothing
  | fn `elem` [FnMul, FnMulu, FnAdd, FnAddu,
               FnOr, FnNor, FnXor]                          = Just $ constWire False
  | fn `elem` [FnSub, FnSubu, FnSlt, FnAnd]                 = Just $ constWire True
  | fn `elem` [FnMfhi, FnMflo]                              = Nothing
  | fn `elem` [FnDiv, FnDivu]                               = undefined -- TODO
  | fn `elem` [FnSll, FnSlr, FnSra, FnSllv, FnSrlv, FnSrav] = undefined -- TODO

opcodeAluNegateOutput :: MonadCircuit m => (Wire m) -> Opcode -> Maybe (Wire m)
opcodeAluNegateOutput f OpArith                             = Just f
opcodeAluNegateOutput f OpUnknown                           = Nothing
opcodeAluNegateOutput f op
  | op `elem` [OpJ, OpJal, OpBeq, OpBne, OpSw, OpLui, OpLw] = Nothing
  | op `elem` [OpAddi, OpAddiu, OpOri]                      = Just $ constWire False
  | op `elem` [OpAndi]                                      = Just $ constWire True
  | op `elem` [OpJal]                                       = undefined -- TODO : add from PC ?

functAluNegateOutput :: MonadCircuit m => Funct -> Maybe (Wire m)
functAluNegateOutput FnUnknown                            = Nothing
functAluNegateOutput fn
  | fn `elem` [FnSyscall, FnJr]                             = Nothing
  | fn `elem` [FnMul, FnMulu, FnAdd, FnAddu, FnSub,
               FnSubu, FnSlt, FnOr, FnXor]                  = Just $ constWire False
  | fn `elem` [FnAnd, FnNor]                                = Just $ constWire True
  | fn `elem` [FnMfhi, FnMflo]                              = Nothing
  | fn `elem` [FnDiv, FnDivu]                               = undefined -- TODO
  | fn `elem` [FnSll, FnSlr, FnSra, FnSllv, FnSrlv, FnSrav] = undefined -- TODO

opcodeAluInitialCarry :: MonadCircuit m => (Wire m) -> Opcode -> Maybe (Wire m)
opcodeAluInitialCarry f OpArith                             = Just f
opcodeAluInitialCarry f OpUnknown                           = Nothing
opcodeAluInitialCarry f op
  | op `elem` [OpJ, OpJal, OpBeq, OpBne, OpSw, OpLui, OpLw] = Nothing
  | op `elem` [OpAddi, OpAddiu, OpAndi, OpOri]              = Just $ constWire False
  | op `elem` [OpJal]                                       = undefined -- TODO : add from PC ?

functAluInitialCarry :: MonadCircuit m => Funct -> Maybe (Wire m)
functAluInitialCarry FnUnknown                            = Nothing
functAluInitialCarry fn
  | fn `elem` [FnSyscall, FnJr]                             = Nothing
  | fn `elem` [FnAdd, FnAddu]                               = Just $ constWire False
  | fn `elem` [FnSub, FnSubu, FnSlt]                        = Just $ constWire True
  | fn `elem` [FnMul, FnMulu, FnOr, FnXor]                  = Nothing
  | fn `elem` [FnMfhi, FnMflo]                              = Nothing
  | fn `elem` [FnDiv, FnDivu]                               = undefined -- TODO
  | fn `elem` [FnSll, FnSlr, FnSra, FnSllv, FnSrlv, FnSrav] = undefined -- TODO



-- |'Instruction' contains the different parts of a MIPS instruction
data Instruction m = Instruction
                     { instructionOpcode    :: Vec (N 6) (Wire m)
                     , instructionRs        :: RegisterIndex m
                     , instructionRt        :: RegisterIndex m
                     , instructionRd        :: RegisterIndex m
                     , instructionShamt     :: Vec (N 5) (Wire m)
                     , instructionFunct     :: Vec (N 6) (Wire m)
                     , instructionImmediate :: Vec (N 16) (Wire m)
                     , instructionAddress   :: Vec (N 26) (Wire m)
                     , instructionRsValue   :: V32 m
                     , instructionRtValue   :: V32 m
                     }

-- |'readInstruction' extracts the useful components of a 32 bits MIPS instruction
readInstruction :: MonadCircuit m => Registers m -> Circuit m (V32 m) (Instruction m)
readInstruction regs = proc (o :| i) -> do
  let rs :| rt :| i'       = i
  let rd :| shamt :| funct = i'
  rsValue <- select -< (regs, rs)
  rtValue <- select -< (regs, rt)
  returnA -< Instruction o rs rt rd shamt funct i' i rsValue rtValue

-- TODO : Move these circuits to a single one to improve OBDD syn ?

instructionRegisterWriteEnable :: MonadCircuit m => Circuit m (Instruction m) (Wire m)
instructionRegisterWriteEnable = proc i -> do
  fWe <- fromTable (functRegisterWriteEnable <$> functTable) -< instructionFunct i
  fromTable (opcodeRegisterWriteEnable fWe <$> opcodeTable) -<< instructionOpcode i

instructionDestinationRegister :: MonadCircuit m => Circuit m (Instruction m) (RegisterIndex m)
instructionDestinationRegister = proc i -> do
  fRe <- fromTable (functDestinationRegister (instructionRd i) <$> functTable) -<< instructionFunct i
  fromTable (opcodeDestinationRegister fRe (instructionRd i) <$> opcodeTable) -<< instructionOpcode i

instructionMemoryWriteEnable :: MonadCircuit m => Circuit m (Instruction m) (Wire m)
instructionMemoryWriteEnable = proc i -> do
  fromTable (opcodeMemoryWriteEnable <$> opcodeTable) -< instructionOpcode i

instructionAluInput :: MonadCircuit m => V64 m -> Circuit m (Instruction m) (AluInput m)
instructionAluInput hilo = proc i -> do
  fAluOperation    <- fromTable (functAluOperation <$> functTable)                       -< instructionFunct i
  aluOperation     <- fromTable (opcodeAluOperation fAluOperation <$> opcodeTable)       -<< instructionOpcode i
  fAluInput1       <- fromTable (functAluInput1 i <$> functTable)                        -<< instructionFunct i
  aluInput1        <- fromTable (opcodeAluInput1 fAluInput1 i <$> opcodeTable)           -<< instructionOpcode i
  fAluInput2       <- fromTable (functAluInput2 i <$> functTable)                        -<< instructionFunct i
  aluInput2        <- fromTable (opcodeAluInput2 fAluInput2 i <$> opcodeTable)           -<< instructionOpcode i
  fAluNegateInput1 <- fromTable (functAluNegateInput1 <$> functTable)                    -< instructionFunct i
  aluNegateInput1  <- fromTable (opcodeAluNegateInput1 fAluNegateInput1 <$> opcodeTable) -<< instructionOpcode i
  fAluNegateInput2 <- fromTable (functAluNegateInput2 <$> functTable)                    -< instructionFunct i
  aluNegateInput2  <- fromTable (opcodeAluNegateInput2 fAluNegateInput2 <$> opcodeTable) -<< instructionOpcode i
  fAluNegateOutput <- fromTable (functAluNegateOutput <$> functTable)                    -< instructionFunct i
  aluNegateOutput  <- fromTable (opcodeAluNegateOutput fAluNegateOutput <$> opcodeTable) -<< instructionOpcode i
  fAluInitialCarry <- fromTable (functAluInitialCarry <$> functTable)                    -< instructionFunct i
  aluInitialCarry  <- fromTable (opcodeAluInitialCarry fAluInitialCarry <$> opcodeTable) -<< instructionOpcode i
  returnA -< AluInput aluOperation aluInput1 aluInput2 aluNegateInput1 aluNegateInput2 aluNegateOutput aluInitialCarry hilo
