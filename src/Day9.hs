module Day9 where

import Relude

type Problem = [[Int]]

parse :: Text -> Problem
parse = fmap (mapMaybe (readMaybe . toString) . words) . lines

delta :: [Int] -> [Int]
delta = fromMaybe [] . viaNonEmpty delta'
  where
    delta' l = zipWith (-) (tail l) (toList l)

allDeltas :: [Int] -> [[Int]]
allDeltas = takeWhile (any (/= 0)) . iterate delta

next, prev :: [Int] -> Int
next = sum . fmap last . mapMaybe nonEmpty . allDeltas
prev = sum . zipWith (*) (cycle [1, -1]) . (fmap head . mapMaybe nonEmpty . allDeltas)

part1, part2 :: Problem -> Int
part1 = sum . fmap next
part2 = sum . fmap prev

run :: (Problem -> a) -> FilePath -> IO a
run f = (f . parse . decodeUtf8 <$>) . readFileBS

run1, run2 :: FilePath -> IO Int
run1 = run part1
run2 = run part2
