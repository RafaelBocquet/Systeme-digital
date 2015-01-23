{-# LANGUAGE Arrows, DataKinds, LambdaCase #-}
module Main where


import Magma.Circuit
import Magma.Vec
import Magma.Nat
import Magma.Instruction
import Magma.Arithmetic
import Magma.ALU
import Magma.CPU

import Magma.Simulation

import Control.Arrow
import Control.Applicative
import Data.Traversable
import Data.Foldable


toBinary' 0 = const False <$> [0 :: Int, 0..]
toBinary' n = (n `mod` 2 /= 0) : toBinary' (n `div` 2)
toBinary  n = take 32 (toBinary' n)

printV32 = foldMap (\case
                       WConst False -> "0"
                       WConst True  -> "1"
                   )
                   

main :: IO ()
main = do
  putStrLn.printV32.head $ runSimulation parallelMult32 [(fromList (constWire <$> toBinary 3), fromList (constWire <$> toBinary 1))]
  return ()
