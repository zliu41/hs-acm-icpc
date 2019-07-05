module CircularDNA where

import           Data.IntMap.Strict ((!?))
import qualified Data.IntMap.Strict as Map
import           Data.List (foldl')

circularDNA :: [String] -> (Int, Int)
circularDNA (readAll -> circle) = fst $ foldl' alg ((1,n), (2,n,initial)) circle
  where
    initial = Map.map fst . Map.filter ((== 0) . snd) $ foldl' (flip initAlg) mempty circle

    initAlg i = flip Map.alter (abs i) \case
      Nothing -> Just $ if i > 0 then (0,1) else (-1,-1)
      Just (def, diff) -> Just $
        let diff' = diff + signum i
         in (min def diff', diff')

    n = Map.size (Map.filter (== 0) initial)

    alg ((besti, best), (i, cur, m)) dna
      | Just def <- m !? abs dna =
          let def' = if dna < 0 then min 0 (def+1) else (def-1)
              cur' | def == 0 && def' < 0 = cur-1
                   | def < 0 && def' == 0 = cur+1
                   | otherwise = cur
              (besti', best') = (if cur' > best then i else besti, max cur' best)
              m' = Map.insert (abs dna) def' m
           in ((besti', best'), (i+1, cur', m'))
      | otherwise = ((besti, best), (i+1, cur, m))

readAll :: [String] -> [Int]
readAll = fmap \case
  ('s':n) -> read n
  ('e':n) -> -read n
  _ -> error "invalid input"
