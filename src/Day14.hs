{-# LANGUAGE NumericUnderscores #-}

module Day14 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Relude

type Problem = [ByteString]

parse :: ByteString -> Problem
parse = BS.split '\n'

-- Our "north" is to the right, since it makes the problem easier
faceNorth :: Problem -> Problem
faceNorth = rotate

move :: Problem -> Problem
move = fmap (BS.intercalate "#" . fmap BS.sort . BS.split '#')

count :: Problem -> [[Int]]
count = fmap (fmap succ . BS.elemIndices 'O')

part1 :: ByteString -> Int
part1 = sum . join . count . move . faceNorth . parse

rotate :: Problem -> Problem
rotate = fmap BS.reverse . BS.transpose

rotMov :: Problem -> Problem
rotMov = rotate . move

doCycle :: Problem -> Problem
doCycle = rotMov . rotMov . rotMov . rotMov

doNcycles :: Int -> Problem -> Problem
doNcycles n = foldl' (.) id (replicate n doCycle)

findCycle :: Problem -> (Int, Int)
findCycle = go 0 M.empty
  where
    go :: Int -> M.Map Problem Int -> Problem -> (Int, Int)
    go n m p =
      let p' = doCycle p
       in case M.lookup p' m of
            Just n' -> (n', n + 1)
            Nothing -> go (n + 1) (M.insert p' n m) p'

part2 :: ByteString -> Int
part2 input =
  let n = 1_000_000_000
      p = faceNorth . parse $ input
      (s, r) = findCycle p
      numRots = s + ((n - s) `mod` (r - s - 1))
   in sum . join . count $ doNcycles numRots p

-- Convenience functions for debugging:

-- Anti-clockwise
rotate' :: Problem -> Problem
rotate' = BS.transpose . fmap BS.reverse

display :: Problem -> IO ()
display = traverse_ (putStrLn . BS.unpack)
