{-# LANGUAGE NoMonomorphismRestriction #-}

-- | An improved sort-and-search solution based on unboxed, mutable vectors.
-- The sort phase uses radix sort and takes O(n * logn).
module FirstOfHerName.RadixSort where

import           Control.Monad.ST (runST)
import           Data.Bifunctor (second)
import           Data.Char (ord)
import           Data.Int (Int8, Int32)
import           Data.Tuple.Extra (both, swap)
import qualified Data.Vector.Algorithms.Radix as Radix
import           Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

-- Prefix doubling, as in suffix array construction.
double :: (Num a, VM.Unbox a) => Vector (a, Int) -> Vector ((a, a), Int)
double xs = V.map f xs
  where
    f (x, parent)
      | parent >= 0 = let (x', parent') = xs ! parent in ((x,x'), parent')
      | otherwise = ((x,-1), parent)

ranked :: Vector (Char, Int) -> Vector Int
ranked inp = V.map fst . fst $ until snd (rankifyPairsOfInts . double . fst) (rankifyChars inp)
  where
    rankifyChars =
      rankify (Radix.passes (0 :: Int8))
              (Radix.size (0 :: Int8))
              ((. fromIntegral @_ @Int8 . ord) . Radix.radix)
    rankifyPairsOfInts =
      rankify (Radix.passes ((0,0) :: (Int32, Int32)))
              (Radix.size   ((0,0) :: (Int32, Int32)))
              ((. both (fromIntegral @_ @Int32)) . Radix.radix)

rankify
  :: forall a b. (Eq a, VM.Unbox a, VM.Unbox b)
  => Int -> Int -> (Int -> a -> Int) -> Vector (a, b) -> (Vector (Int, b), Bool)
rankify passes auxSize radix xs = runST do
  xs' <- V.unsafeThaw (V.indexed xs)
  Radix.sortBy passes auxSize ((. fst . snd) . radix) xs'
  ys <- VM.new (V.length xs)

  let alg (nodup, mb) (idx, (a, b)) = case mb of
        Nothing ->
          VM.write ys idx (0, b) >> pure (True, Just (a, 0))
        Just (a', next)
          | a == a' ->
              VM.write ys idx (next, b) >> pure (False, Just (a, next))
          | otherwise ->
              VM.write ys idx (next+1, b) >> pure (nodup, Just (a, next+1))

  (nodup, _) <- V.foldM alg (True, Nothing) =<< V.unsafeFreeze xs'
  fmap (, nodup) (V.unsafeFreeze ys)

firstOfHerName :: [(Char, Int)] -> [String] -> [Int]
firstOfHerName inp = map ans
  where
    inp' = V.fromList $ map (second (\x -> x-1)) inp
    len = V.length inp'

    ranked' :: Vector (Int, Int)
    ranked' = V.map swap . V.indexed $ ranked inp'

    sorted :: Vector Int
    sorted = V.unsafeAccumulate (const id) (V.replicate len 0) ranked'

    ans :: String -> Int
    ans q = to - from
      where
        from = go (0, len-1) True
        to = go (from, len-1) False
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
        | i < 0 || i >= len = LT
        | c' < c = LT
        | c' > c = GT
        | otherwise = cmp parent cs
      where
        (c',parent) = inp' ! i
