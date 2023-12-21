{-# LANGUAGE TupleSections #-}

module Day21 where

import Control.Monad.ST
import Data.Array.Base
import Data.Array.ST
import qualified Data.ByteString.Char8 as C
import Relude

parse :: C.ByteString -> UArray (Int, Int) Char
parse input = listArray ((0, 0), (n - 1, m - 1)) . C.unpack $ C.concat l
  where
    l = C.lines input
    n = length l
    m = (maybe 0 C.length . viaNonEmpty head) l

findStart :: UArray (Int, Int) Char -> Maybe (Int, Int)
findStart = fmap fst . find ((== 'S') . snd) . assocs

emptyDistances :: (Int, Int) -> UArray (Int, Int) Int
emptyDistances (n, m) = runSTUArray $ newArray ((0, 0), (n, m)) (-1)

bfs :: UArray (Int, Int) Char -> STUArray s (Int, Int) Int -> [(Int, Int, Int)] -> ST s (STUArray s (Int, Int) Int)
bfs _ dist [] = pure dist
bfs arr dist ((x, y, d) : xs)
  | inRange (bounds arr) (x, y) = do
      v <- readArray dist (x, y)
      if v == -1 && arr ! (x, y) /= '#'
        then do
          writeArray dist (x, y) d
          let d' = d + 1
          bfs arr dist (xs <> [(x + 1, y, d'), (x - 1, y, d'), (x, y + 1, d'), (x, y - 1, d')])
        else bfs arr dist xs
  | otherwise = bfs arr dist xs

computeDistances :: UArray (Int, Int) Char -> UArray (Int, Int) Int
computeDistances arr = runST $ do
  dist' <- thawSTUArray $ emptyDistances (n, m)
  res <- bfs arr dist' [(x, y, 0)]
  freezeSTUArray res
  where
    (n, m) = snd . bounds $ arr
    Just (x, y) = findStart arr

count :: UArray (Int, Int) Int -> Int -> Int
count arr d = length . filter (\n -> n <= d && parity n) . elems $ arr
  where
    parity = bool odd even (even d)

part1 :: UArray (Int, Int) Char -> Int
part1 = flip count 64 . computeDistances
