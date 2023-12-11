{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module Day11 where

import Data.ByteString.Char8 (elemIndices, splitWith)
import Data.Foldable (maximum, minimum)
import Data.List ((\\))
import Relude

type Problem = [(Int, Int)]

parse :: ByteString -> Problem
parse = join . zipWith fn [0 ..] . fmap (elemIndices '#') . splitWith (== '\n')
  where
    fn :: Int -> [Int] -> [(Int, Int)]
    fn y xs = (y,) <$> xs

findMissing :: [Int] -> [Int]
findMissing l = let (low, high) = (minimum l, maximum l) in [low .. high] \\ l

distance :: Int -> [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
distance n expandedH expandedW (h1, w1) (h2, w2) = dist1D expandedH h1 h2 + dist1D expandedW w1 w2
  where
    dist1D expanded c1 c2 | c1 > c2 = dist1D expanded c2 c1
    dist1D expanded c1 c2 = c2 - c1 + (n - 1) * length (filter (between c1 c2) expanded)

    between a b x = a <= x && x <= b

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = fmap (x,) xs ++ allPairs xs

solve :: Int -> Problem -> Int
solve n galaxies =
  let (expandedH, expandedW) = join bimap findMissing $ unzip galaxies
   in sum $ uncurry (distance n expandedH expandedW) <$> allPairs galaxies

part1, part2 :: Problem -> Int
part1 = solve 2
part2 = solve 1_000_000

run :: (Problem -> Int) -> FilePath -> IO Int
run f fp = readFileBS fp <&> f . parse

run1, run2 :: FilePath -> IO Int
run1 = run part1
run2 = run part2
