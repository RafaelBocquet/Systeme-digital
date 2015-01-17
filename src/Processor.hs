{-# LANGUAGE Arrows #-}
module Main where

import Magma.Circuit

import Control.Arrow

main :: IO ()
main = do
  putStrLn $ runCircuit $ reg $ \a -> proc () -> do
    b <- not1 -< a
    returnA -< (b, ())
  return ()
