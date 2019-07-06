-- Thinning solution
--
-- This solution gives the optimal bridge, in addition to the optimal cost, for free.
module BeautifulBridges.Thinning where

import Data.Maybe (mapMaybe, maybeToList)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Ord (comparing)
import qualified Data.List.NonEmpty as Nel
import Safe.Foldable (minimumByMay, minimumMay)

beautifulBridges :: [(Int, Int)] -> Int -> Int -> Int -> Maybe Int
beautifulBridges ground h α β = minCost (foldr alg [lastPillar] (init ground))
  where
    lastPillar =
      let (x,y) = last ground
       in Bridge { bridgePillars = pure (x,y)
                 , bridgeCost = archCost y 0
                 , bridgeSup = 2*(h-y)
                 , bridgeInf = 0
                 }

    alg _ [] = []
    alg (x,y) bs = new ++ glue
      where
        new = maybeToList . minimumByMay (comparing bridgeCost) $ mapMaybe (newArch (x,y)) bs
                             -- ↑ thinning
        glue = mapMaybe (glueArch (x,y)) bs

    newArch (x,y) b
        | ok b =  -- create a new arch only if b is valid
            Just Bridge { bridgePillars = (x,y) <| bridgePillars b
                        , bridgeCost = bridgeCost b + archCost y (x'-x)
                        , bridgeSup = sup
                        , bridgeInf = inf
                        }
        | otherwise = Nothing
      where
        (x',y') = Nel.head (bridgePillars b)
        (inf,sup) = bounds (x'-x) (h-y) 0 (2*(h-y'))

    glueArch (x,y) b = case bridgePillars b of
      (_ :| []) -> Nothing  -- no arch to glue to
      ((_,y1) :| ((x2,_) : _))
        | dx > newSup -> Nothing
        | otherwise ->  -- glue to the first arch as long as the diameter does not exceed the max
              Just Bridge { bridgePillars = (x,y) :| Nel.tail (bridgePillars b)
                          , bridgeCost = bridgeCost b - archCost y1 (diameter b) + archCost y dx
                          , bridgeSup = newSup
                          , bridgeInf = newInf
                          }
            where
              (dx,dy) = (x2-x, h-y)
              (newInf, newSup) = bounds dx dy (bridgeInf b) (bridgeSup b)

    bounds dx dy inf sup = (inf',sup')
      where
        inf' = if dy*2 > dx then inf else max inf (r1 - floor r2)
        sup' = min sup (r1 + floor r2)
        -- r1±r2 are the roots of (x-dx)^2 + (x-dy)^2 = x^2.
        (r1,r2) = (2*(dx+dy), sqrt(fromIntegral (8*dx*dy) :: Double))

    archCost y d = α*(h-y) + β*d*d

    minCost = minimumMay . map bridgeCost . filter ok

    ok b = diameter b >= bridgeInf b && diameter b <= bridgeSup b

data Bridge = Bridge
  { bridgePillars :: NonEmpty (Int, Int)
  , bridgeCost :: !Int
  , bridgeSup :: !Int
  -- ^ diameter upper bound for the first arch
  , bridgeInf :: !Int
  -- ^ diameter lower bound for the first arch
  } deriving (Show)

-- | Diameter of the first arch.
diameter :: Bridge -> Int
diameter b = case bridgePillars b of
  (x,_) :| ((x',_) : _) -> x'-x
  _ -> 0
