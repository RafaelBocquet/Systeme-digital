{-# LANGUAGE Arrows, DataKinds #-}
module Main where

import Magma.Circuit
import Magma.Vec
import Magma.Nat
import Magma.Instruction
import Magma.Arithmetic

import Control.Arrow
import Data.Traversable

main :: IO ()
main = do
  putStrLn $ runCircuit $ registerLike $ \a -> proc () -> do
    let c :| d = a :: Vec (N 2) Wire
        c' :> _ = c
        d' :> _ = d
        _ = c :: Vec (N 1) Wire
        _ = d :: Vec (N 1) Wire
    b <- not1 -< c'
    returnA -< (b `VCons` b `VCons` VNil, ())
  return ()
