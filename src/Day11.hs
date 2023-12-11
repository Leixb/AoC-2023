{-# LANGUAGE TupleSections #-}

module Day11 where

import Data.Array.IArray
import qualified Data.Text as T
import Relude

type Grid = Array (Int, Int) Bool

parse :: Text -> Maybe Grid
parse l = do
  let ls = lines l
      h = length ls
  w <- T.length <$> viaNonEmpty head ls

  pure $ listArray ((1, 1), (h, w)) $ (== '#') <$> T.unpack (T.concat ls)

findEmptyRows, findEmptyCols :: Grid -> [Int]
findEmptyRows g = let ((h1, w1), (h2, w2)) = bounds g in [h | h <- [h1 .. h2], not $ any (g !) [(h, w) | w <- [w1 .. w2]]]
findEmptyCols g = let ((h1, w1), (h2, w2)) = bounds g in [w | w <- [w1 .. w2], not $ any (g !) [(h, w) | h <- [h1 .. h2]]]

allLocations :: Grid -> [(Int, Int)]
allLocations g = let ((h1, w1), (h2, w2)) = bounds g in [(h, w) | h <- [h1 .. h2], w <- [w1 .. w2], g ! (h, w)]

distance :: [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
-- distance emptyC emptyR (h1, w1) (h2, w2) | h1 > h2 = distance emptyC emptyR (h2, w1) (h1, w2)
-- distance emptyC emptyR (h1, w1) (h2, w2) | w1 > w2 = distance emptyC emptyR (h1, w2) (h2, w1)
distance emptyC emptyR (h1, w1) (h2, w2) = distCoord emptyC h1 h2 + distCoord emptyR w1 w2

distance' :: Int -> [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
distance' n emptyC emptyR (h1, w1) (h2, w2) = distCoord' n emptyC h1 h2 + distCoord' n emptyR w1 w2

distCoord :: [Int] -> Int -> Int -> Int
distCoord emptyC c1 c2 | c1 > c2 = distCoord emptyC c2 c1
distCoord emptyC c1 c2 = c2 - c1 + length (filter (\c -> c1 < c && c < c2) emptyC)

distCoord' :: Int -> [Int] -> Int -> Int -> Int
distCoord' n emptyC c1 c2 | c1 > c2 = distCoord' n emptyC c2 c1
distCoord' n emptyC c1 c2 = c2 - c1 + (n - 1) * length (filter (\c -> c1 < c && c < c2) emptyC)

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = map (x,) xs ++ allPairs xs

part1 g =
  let galaxies = allLocations g
      emptyC = findEmptyCols g
      emptyR = findEmptyRows g

      pairs = allPairs galaxies
      pairIds = allPairs [1 .. length galaxies]
      res = map (uncurry $ distance emptyR emptyC) pairs
   in traceShow (zip pairIds res) res

printGrid :: Maybe Grid -> IO ()
printGrid (Just g) = do
  let ((h1, w1), (h2, w2)) = bounds g
  forM_ [h1 .. h2] $ \h -> do
    forM_ [w1 .. w2] $ \w -> do
      putStr $ if g ! (h, w) then "#" else "."
    putStrLn ""
printGrid Nothing = putStrLn "Invalid grid"

part2 n g =
  let galaxies = allLocations g
      emptyC = findEmptyCols g
      emptyR = findEmptyRows g

      pairs = allPairs galaxies
      res = map (uncurry $ distance' n emptyR emptyC) pairs
   in res
