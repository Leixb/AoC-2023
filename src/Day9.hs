module Day9 where

import Control.Arrow
import Relude

type Problem = [[Int]]

parse :: Text -> Problem
parse = fmap (mapMaybe (readMaybe . toString) . words) . lines

part1 :: Problem -> Int
part1 = sum . fmap getNext

delta :: NonEmpty Int -> [Int]
delta = tail &&& toList >>> uncurry (zipWith (-))

delta' :: [Int] -> [Int]
delta' = fromMaybe [] . viaNonEmpty delta

doDeltas :: [Int] -> [[Int]]
doDeltas = takeWhile (any (/= 0)) . iterate delta'

getNext :: [Int] -> Int
getNext = sum . fmap last . mapMaybe nonEmpty . doDeltas

run1 f = readFileBS f <&> part1 . parse . decodeUtf8

run2 f = readFileBS f <&> part2 . parse . decodeUtf8

part2 :: Problem -> Int
part2 = sum . fmap getPrev

getPrev :: [Int] -> Int
getPrev l = sum $ zipWith (*) (fmap head . mapMaybe nonEmpty . doDeltas $ l) $ cycle [1, -1]
