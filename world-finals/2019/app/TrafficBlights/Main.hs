{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Foldable (traverse_)
import Text.Printf (printf)

import TrafficBlights.Common
import TrafficBlights.DivideAndConquer


main :: IO ()
main = do
  n <- readLn
  lights <- replicateM n do
    ln <- getLine
    let [x, r, g] = read <$> words ln
    pure (Light x r g)
  let (ps, p) = trafficBlights lights
  traverse_ (printf "%.9f\n") (ps ++ [p])
