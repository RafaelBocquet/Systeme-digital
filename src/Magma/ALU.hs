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

type AluOperation = Vec (N 2) Wire

aluAndOperation, aluOrOperation, aluAddOperation, aluMulOperation :: AluOperation
aluAndOperation = fmap WConst $ False `VCons` False `VCons` VNil
aluOrOperation  = fmap WConst $ False `VCons` True `VCons` VNil
aluAddOperation = fmap WConst $ True `VCons` False `VCons` VNil
aluMulOperation = fmap WConst $ True `VCons` True `VCons` VNil

data AluInput = AluInput
                { aluOperation   :: AluOperation
                , aluI1          :: V32 -- ^ first ALU input
                , aluI2          :: V32 -- ^ second ALU input
                , aluNegateInput :: Wire -- ^ 1 if the second input should be negated (e.g. for sub)
                , aluCarryBit    :: Wire -- ^ The initial carry bit for the adder (0 for add, 1 for sub)
                , aluIMUL        :: Vec (N 64) Wire -- ^ The old (LO HI) register, to be kept unless the operation is a mult
                }

data AluOutput = AluOutput
               { aluO    :: V32 -- ^ The ALU output for operations and, or and add.
               , aluOMUL :: Vec (N 64) Wire -- ^ The new (LO HI) register, updated if the operation was a mult
               }


-- Do all operations
-- aluO <- fromTable $(fromList [| [Just andRes, Just orRes, Just addRes, Nothing] |]) -<< aluOperation aluInput
-- aluOMul <- fromTable $(fromList [| [Nothing, Nothing, Nothing, Just mulRes] |]) -<< aluOperation aluInput
alu :: Circuit AluInput AluOutput
alu = undefined
