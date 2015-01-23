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

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

import Magma.Circuit
import Magma.Vec
import Magma.Nat

type Registers m     = Vec (N 32) (V32 m)
type RegisterIndex m = Vec (N 5) (Wire m)
type V32 m           = Vec (N 32) (Wire m)
type PC m            = V32 m
type V64 m           = Vec (N 64) (Wire m)

-- | Template Haskell to get a 'RegisterIndex' from an Int literal
-- > $(registerIndex 30) == True `VCons` True `VCons` True `VCons` True `VCons` False
registerIndex :: Int -> ExpQ
registerIndex i
  | i < 0 || i > 31 = fail "Register index should be positive and less than 31"
  | otherwise       = do
      let toBinary' 0 = const False <$> [0, 0..]
          toBinary' n = (n `mod` 2 /= 0) : toBinary' (n `div` 2)
          toBinary  n = reverse (take 5 (toBinary' n))
      inlineVec$ (lift (toBinary i))

-- | binaryToUnary -< r transforms r into its unary representation
binaryToUnary :: MonadCircuit m => Circuit m (Vec n (Wire m)) (Vec (P2 n) (Wire m))
binaryToUnary = undefined


-- | updateRegister -< ((we, v), (re, reg)) update reg with value v when we and re are 1
updateSingleRegister :: MonadCircuit m => Circuit m ((Wire m, V32 m), (Wire m, V32 m)) (V32 m)
updateSingleRegister = proc ((we, v), (re, oldv)) -> do
  enable <- and2 -< (we, re)
  mux -< (enable, v, oldv)

-- | updateRegister -< (we, regs, ri, v) updates the register with index ri in regs with value v when we is 1
updateRegister :: MonadCircuit m => Circuit m (Wire m, Registers m, RegisterIndex m, V32 m) (Registers m)
updateRegister = proc (we, regs, ri, v) -> do
  re <- binaryToUnary -< ri
  bitwise updateSingleRegister -<
    zip (replicateVec (we, v)) (zip re regs)
