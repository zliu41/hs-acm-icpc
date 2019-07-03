module TrafficBlights.Common where

import Control.Arrow ((&&&))

data Light = Light
  { pos :: Int
  , red :: Int
  , green :: Int
  } deriving (Eq, Show)

period :: Light -> Int
period = uncurry (+) . (red &&& green)
