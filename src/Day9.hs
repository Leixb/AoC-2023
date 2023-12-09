module Day9 where

import Relude

type Problem = [[Int]]

parse :: Text -> Maybe Problem
parse = traverse (traverse (readMaybe . toString) . words) . lines

delta :: [Int] -> [Int]
delta = fromMaybe [] . viaNonEmpty delta'
  where
    delta' l = zipWith (-) (tail l) (toList l)

allDeltas :: [Int] -> [NonEmpty Int]
allDeltas = mapMaybe nonEmpty . takeWhile (any (/= 0)) . iterate delta

next, prev :: [Int] -> Int
next = allDeltas >>> sum . fmap last
prev = allDeltas >>> foldr ((-) . head) 0

part1, part2 :: Problem -> Int
part1 = sum . fmap next
part2 = sum . fmap prev

run :: (Problem -> a) -> FilePath -> IO (Maybe a)
run f = (fmap f . parse . decodeUtf8 <$>) . readFileBS

run1, run2 :: FilePath -> IO (Maybe Int)
run1 = run part1
run2 = run part2
