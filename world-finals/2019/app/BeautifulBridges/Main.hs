{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad (replicateM)

import BeautifulBridges

main :: IO ()
main = do
  [n, h, α, β] <- fmap read . words <$> getLine
  ground <- replicateM n do
    [x, y] <- fmap read . words <$> getLine
    pure (x, y)
  putStrLn . maybe "impossible" show $ beautifulBridges ground h α β
