{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main (main) where

import CircularDNA

main :: IO ()
main = do
  input <- getLine >> fmap words getLine
  let (i,c) = circularDNA input
  putStrLn $ show i ++ " " ++ show c
