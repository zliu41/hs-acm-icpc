module TrafficBlights.BruteForce where

import           TrafficBlights.Common (Light)
import qualified TrafficBlights.Common as Light

trafficBlights :: [Light] -> ([Double], Double)
trafficBlights = aggregate . results

results :: [Light] -> [[Bool]]
results ls = map (flip resultFor ls) [0..lcmP ls - 1]

resultFor :: Int -> [Light] -> [Bool]
resultFor t0 = reverse . fst . foldr f ([], False) . reverse
                                        -- ↑ whether there is a previous red light
  where
    f l (bs, b) =
      let t = t0 + Light.pos l
          b' = b || t `mod` Light.period l >= Light.red l
       in (b':bs, b || not b')

-- | Overflow doesn't matter, since if it overflows
-- the brute force approach won't terminate anyway.
lcmP :: [Light] -> Int
lcmP = foldr (lcm . Light.period) 1

aggregate :: [[Bool]] -> ([Double], Double)
aggregate bss = (ps, p)
  where
    len = fromIntegral (length bss)
    ps = foldr combineAll initial bss
    initial = repeat 0
    combineAll = zipWith combineOne
    combineOne b d = if b then d else d + 1 / len
    p = fromIntegral (length (filter and bss)) / len
