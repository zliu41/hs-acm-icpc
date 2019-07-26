{-# LANGUAGE NoMonomorphismRestriction #-}

-- | A simple sort-and-search solution. The sort phase takes O(n * logn * logn).
-- Not really optimized for performance, and not fast enough for the
-- most vicious test cases.
module FirstOfHerName.Simple where

import           Data.Array ((!))
import qualified Data.Array.IArray as A
import           Data.Bifunctor (first)
import           Data.Functor ((<&>))
import           Data.Ix (inRange)
import qualified Data.List as List

type Array a = A.Array Int a

-- Prefix doubling, as in suffix array construction.
double :: Array (Int, Int) -> Array ((Int, Int), Int)
double xs = xs <&> \(x, parent) ->
  if inRange (A.bounds xs) parent  -- has a parent, i.e., not the first lady
    then let (x', parent') = xs ! parent in ((x,x'), parent')
    else ((x,0), parent)

firstOfHerName :: [(Char, Int)] -> [String] -> [Int]
firstOfHerName inp = map ans
  where
    len = length inp
    array = A.array (1, len)
    listArray = A.listArray (1, len)
    inp' = listArray inp
    bounds = A.bounds @A.Array inp'

    -- Compute the rank of each element based on the 'a' values.
    -- The boolean indicates whether all 'a' values are distinct.
    rankify :: forall a b. Ord a => Array (a, b) -> (Array (Int, b), Bool)
    rankify ys = (arr, snd zs)
      where
        zs :: ([(((a, b), Int), Int)], Bool)
        zs = first reverse . fst . List.foldl' alg (([], True), Nothing)
             . List.sortOn (fst . fst)
             $ zip (A.elems ys) [1 :: Int ..]
           where
             alg ((acc, nodup), mb) y@((x, _), _) = case mb of
               Nothing -> (((y, 1):acc, True), Just (x, 1))
               Just (x', next)
                 | x == x' -> (((y, next):acc, False), Just (x, next))
                 | otherwise -> (((y, next+1):acc, nodup), Just (x, next+1))

        arr :: Array (Int, b)
        arr = array $ fmap f (fst zs)
          where
            f (((_, x), oldIdx), newIdx) = (oldIdx, (newIdx, x))

    -- ranked[i] == j  ≡  the rank of the ith lady is j
    -- sorted[i] == j  ≡  the rank of the jth lady is i
    ranked, sorted :: Array Int
    ranked = fmap fst . fst $ until snd (rankify . double . fst) (rankify inp')
    sorted = array $ A.elems ranked `zip` [1..]

    ans :: String -> Int
    ans q = to - from
      where
        from = go bounds True
        to = go (from, snd bounds) False
        -- binary search on 'sorted'
        go :: (Int, Int) -> Bool -> Int
        go (inf, sup) goLeft
          | inf > sup = inf
          | otherwise =
              let i = (inf + sup) `div` 2
               in case cmp (sorted ! i) q of
                    EQ -> if goLeft then go (inf, i-1) goLeft else go (i+1, sup) goLeft
                    LT -> go (i+1, sup) goLeft
                    GT -> go (inf, i-1) goLeft

    -- compare the name of the ith lady with the string
    cmp :: Int -> String -> Ordering
    cmp _ [] = EQ
    cmp i (c:cs)
        | not (inRange bounds i) = LT
        | c' < c = LT
        | c' > c = GT
        | otherwise = cmp parent cs
      where
        (c',parent) = inp' ! i
