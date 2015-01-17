{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Magma.BDD where

data BDD b = BDDConst Bool
           | BDDCase Int b b
           deriving (Eq, Ord, Generic, Functor, Foldable, Traversable)
