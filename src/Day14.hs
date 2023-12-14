{-# LANGUAGE NumericUnderscores #-}

module Day14 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Relude

type Problem = [ByteString]

-- We apply rotation so that north is to the right, this makes
-- all computations easier since we can just sort the rows.
parse :: ByteString -> Problem
parse = appEndo rotate . BS.split '\n'

count :: Problem -> [[Int]]
count = fmap (fmap succ . BS.elemIndices 'O')

rotate, move, doCycle :: Endo Problem
rotate = Endo $ fmap BS.reverse . BS.transpose
move = Endo $ fmap (BS.intercalate "#" . fmap BS.sort . BS.split '#')
doCycle = stimes (4 :: Int) $ rotate <> move

findCycle :: Problem -> (Int, Int)
findCycle = go 0 M.empty
  where
    go :: Int -> M.Map Problem Int -> Problem -> (Int, Int)
    go n m p = case M.lookup p' m of
      Just n' -> (n', n + 1)
      Nothing -> go (n + 1) (M.insert p' n m) p'
      where
        p' = appEndo doCycle p

part1, part2 :: ByteString -> Int
part1 = sum . join . count . appEndo move . parse
part2 input = sum . join . count $ appEndo (stimes numRots doCycle) p
  where
    n = 1_000_000_000
    p = parse input
    (s, r) = findCycle p
    numRots = s + ((n - s) `mod` (r - s - 1))

-- Convenience functions for debugging:

-- Anti-clockwise
rotate' :: Problem -> Problem
rotate' = BS.transpose . fmap BS.reverse

display :: Problem -> IO ()
display = traverse_ (putStrLn . BS.unpack)
