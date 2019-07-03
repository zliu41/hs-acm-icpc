module TrafficBlights.DivideAndConquer where

import           Control.Arrow ((&&&))
import           Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import           Data.List (find, foldl', mapAccumL)
import           Data.Maybe (fromMaybe)
import           TrafficBlights.Common (Light)
import qualified TrafficBlights.Common as Light

trafficBlights :: [Light] -> ([Double], Double)
trafficBlights ls = (ds, d)
  where (ds, d) = (probs ls, 1 - sum ds)

magic :: Num a => a
magic = 2520

reducedPeriod :: Light -> Int
reducedPeriod = uncurry div . (Light.period &&& gcd magic . Light.period)

primeFactor :: Int -> Int
primeFactor n = fromMaybe n (find ((== 0) . mod n) [2,3,5,7])

probs :: [Light] -> [Double]
probs ls = foldr (\i -> zipWith (+) (probsFor i ls)) (repeat 0) [0..magic-1]

probsFor :: Int -> [Light] -> [Double]
probsFor i = snd . mapAccumL (accumAlgebra i) (Set.empty, 1 / magic)

accumAlgebra :: Int -> (IntSet, Double) -> Light -> ((IntSet, Double), Double)
accumAlgebra i (prevReds, probAllGreen) l = ((prevReds', probAllGreen'), probFirstRed)
  where
    p = reducedPeriod l
    pf = primeFactor p
    store = (+ pf) . (* 100)
    (total, reds, ks) = foldl' f (0 :: Int, 0, []) [0..p-1]

    f acc@(total', reds', ks') k
      | store k `Set.member` prevReds = acc
      | t `mod` Light.period l < Light.red l = (total'+1, reds'+1, store k : ks')
      | otherwise = (total'+1, reds', ks')
      where t = i + k * magic + Light.pos l

    prevReds' = Set.fromList ks `Set.union` prevReds

    probAllGreen'
      | total > 0 = probAllGreen * fromIntegral (total - reds) / fromIntegral total
      | otherwise = probAllGreen

    probFirstRed = probAllGreen - probAllGreen'
