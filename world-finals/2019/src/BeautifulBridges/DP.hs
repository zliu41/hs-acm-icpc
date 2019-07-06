-- | Dynamic programming solution
module BeautifulBridges.DP where

import Data.List as List
import Data.Maybe (catMaybes)
import Data.Vector ((!))
import qualified Data.Vector as V
import Safe.Foldable (minimumMay)

beautifulBridges :: [(Int, Int)] -> Int -> Int -> Int -> Maybe Int
beautifulBridges ground h α β = V.head memo
  where
    ground' = V.fromList ground

    memo = V.map bestFor . V.indexed . V.fromList . drop 1 $ List.tails ground

    bestFor (i, xys)
      | i == V.length ground' - 1 = Just (α*(h-y))
      | otherwise = minimumMay . catMaybes $ go [] i (i+1) 0 (2*(h-y)) xys
      where (_,y) = ground' ! i

    go r i j inf sup = \case
      [] -> r
      ((x',y'): xys)
        | dx >= inf' && dx <= sup' ->
            let c = fmap (+ cost i x') (memo ! j)
             in go (c:r) i (j+1) inf' sup' xys
        | inf' > sup' || dx > sup' -> r
        | otherwise -> go r i (j+1) inf' sup' xys
        where
          (x,_) = ground' ! i
          (dx,dy) = (x'-x, h-y')
          inf' = if dy*2 > dx then inf else max inf (r1 - floor r2)
          sup' = min sup (r1 + floor r2)
          -- r1±r2 are the roots of (x-dx)^2 + (x-dy)^2 = x^2.
          (r1,r2) = (2*(dx+dy), sqrt(fromIntegral (8*dx*dy) :: Double))

    cost i x' = α*(h-y) + β*d*d
      where (x,y) = ground' ! i
            d = x'-x
