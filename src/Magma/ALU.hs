{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms #-}

module Magma.ALU where

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
import Magma.Register
import Magma.Arithmetic

type AluOperation m = Vec (N 2) (Wire m)

aluXorOperation, aluOrOperation, aluAddOperation, aluMulOperation :: MonadCircuit m => AluOperation m
aluXorOperation = fmap constWire $ False `VCons` False `VCons` VNil
aluOrOperation  = fmap constWire $ False `VCons` True `VCons` VNil
aluAddOperation = fmap constWire $ True `VCons` False `VCons` VNil
aluMulOperation = fmap constWire $ True `VCons` True `VCons` VNil

data AluInput m = AluInput
                  { aluOperation    :: AluOperation m
                  , aluI1           :: V32 m -- ^ first ALU input
                  , aluI2           :: V32 m -- ^ second ALU input
                  , aluNegateInput1 :: Wire m -- ^ 1 if the first input should be negated (e.g. for and)
                  , aluNegateInput2 :: Wire m -- ^ 1 if the second input should be negated (e.g. for sub)
                  , aluNegateOutput :: Wire m -- ^ 1 if the output should be negated (e.g. for nor)
                  , aluInitialCarry :: Wire m -- ^ The initial carry bit for the adder (0 for add, 1 for sub)
                  , aluIMUL         :: Vec (N 64) (Wire m) -- ^ The old (LO HI) register, to be kept unless the operation is a mult
                  }

data AluOutput m = AluOutput
                   { aluO    :: V32 m -- ^ The ALU output for operations and, or and add.
                   , aluOMUL :: Vec (N 64) (Wire m) -- ^ The new (LO HI) register, updated if the operation was a mult
                   }


-- Do all operations
-- aluO <- fromTable $(fromList [| [Just andRes, Just orRes, Just addRes, Nothing] |]) -<< aluOperation aluInput
-- aluOMul <- fromTable $(fromList [| [Nothing, Nothing, Nothing, Just mulRes] |]) -<< aluOperation aluInput
alu :: Circuit m (AluInput m) (AluOutput m)
alu = undefined
