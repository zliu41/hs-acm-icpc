{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Foldable (traverse_)

import FirstOfHerName.Simple

main :: IO ()
main = do
  [n, k] <- fmap read . words <$> getLine
  ladies <- replicateM n do
    [c, p] <- words <$> getLine
    pure (head c, read p)
  queries <- replicateM k getLine
  traverse_ print (firstOfHerName ladies queries)
