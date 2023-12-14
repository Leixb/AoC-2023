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

-- Anti-clockwise
rotate' :: Problem -> Problem
rotate' = BS.transpose . fmap BS.reverse

display :: Problem -> IO ()
display = traverse_ (putStrLn . BS.unpack)

rotMov :: Problem -> Problem
rotMov = rotate . move

doCycle :: Problem -> Problem
doCycle = rotMov . rotMov . rotMov . rotMov

doNcycles :: Int -> Problem -> Problem
doNcycles n = foldr (.) id (replicate n doCycle)

-- Iteration number, problem state and map of states to iteration numbers
type StateType = (Int, Problem, M.Map Problem Int)

doCycles :: State StateType (Int, Int)
doCycles = do
  (n, p, m) <- get
  let p' = doCycle p
  case M.lookup p' m of
    Just n' -> pure (n', n + 1)
    Nothing -> do
      put (n + 1, p', M.insert p' n m)
      doCycles

initState :: Problem -> StateType
initState p = (0, p, M.singleton p 0)

findCycle :: Problem -> (Int, Int)
findCycle = evalState doCycles . initState

part2 :: ByteString -> Int
part2 = part2' 1_000_000_000

part2' :: Int -> ByteString -> Int
part2' n input =
  let (s, r) = findCycle p
      p = faceNorth . parse $ input
      numRots = s + ((n - s) `mod` (r - s - 1))
   in sum . join . count $ doNcycles numRots p
