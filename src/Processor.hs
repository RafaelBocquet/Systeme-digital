{-# LANGUAGE Arrows, DataKinds #-}
module Main where

import Magma.Circuit
import Magma.Vec
import Magma.Nat
import Magma.Instruction
import Magma.Arithmetic
import Magma.ALU
import Magma.CPU

import Control.Arrow
import Data.Traversable

main :: IO ()
main = do
  putStrLn $ runCircuit parallelAdder32
  return ()
