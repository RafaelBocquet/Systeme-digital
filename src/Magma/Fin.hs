{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, GADTs, DataKinds, PolyKinds, TypeOperators, StandaloneDeriving, TypeFamilies, UndecidableInstances, ViewPatterns, PatternSynonyms, MultiParamTypeClasses, ScopedTypeVariables #-}

module Magma.Fin where

import qualified GHC.TypeLits
import Magma.Nat

data Fin n where
  FZ :: Fin n
  FS :: Fin n -> Fin (S n)

deriving instance Eq (Fin n)
deriving instance Ord (Fin n)
