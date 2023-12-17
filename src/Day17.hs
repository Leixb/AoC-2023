module Day17 where

import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt)
import Data.Heap hiding (filter)
import qualified Data.Heap as H
import Relude

type Pos = (Int, Int)

type Grid = UArray Pos Int

data Dir = U | D | L | R deriving (Eq, Ord, Show, Enum, Bounded, Ix)

parse :: ByteString -> Maybe Grid
parse input = do
  let l = fmap (fmap digitToInt . BS.unpack) . BS.lines $ input
      h = length l
  w <- fmap length . viaNonEmpty head $ l
  pure . listArray ((0, 0), (w - 1, h - 1)) . concat $ l

move :: Dir -> Pos -> Pos
move U = first pred
move D = first succ
move L = second pred
move R = second succ

nextDir :: Dir -> [Dir]
nextDir U = [L, R]
nextDir D = [L, R]
nextDir L = [U, D]
nextDir R = [U, D]

-- position, previous direction, accumulated loss
type S = (Int, Pos, Dir)

doMove :: Grid -> Dir -> S -> Maybe S
doMove g d (c, p, _) = do
  let p' = move d p
  guard $ inRange (bounds g) p'
  pure (c + g ! p', p', d)

doMoveN :: Grid -> Dir -> Int -> S -> Maybe S
doMoveN g d n = foldl' (>=>) pure . replicate n $ doMove g d

doMoves :: Grid -> [Int] -> S -> Dir -> [S]
doMoves g r s d = mapMaybe (flip (doMoveN g d) s) r

allMoves :: Grid -> [Int] -> S -> [S]
allMoves g r s@(_, _, prev) = nextDir prev >>= doMoves g r s

solve' :: Grid -> [Int] -> UArray (Pos, Dir) Int -> Pos -> MinHeap S -> Maybe Int
solve' g r distances target h = do
  ((acc, pos, dir), h') <- H.view h

  if pos == target
    then pure acc
    else do
      let moves = allMoves g r (acc, pos, dir)
          moves' = filter (\(acc, p, d) -> acc < distances ! (p, d)) moves
          distances' = distances // fmap (\(acc, p, d) -> ((p, d), acc)) moves'
          h'' = foldl' (flip H.insert) h' moves'
      solve' g r distances' target h''

solve :: Grid -> [Int] -> Maybe Int
solve g r = solve' g r (emptyGrid ((lo, minBound), (hi, maxBound))) hi $ H.singleton (0, (0, 0), U)
  where
    (lo, hi) = bounds g
    emptyGrid = flip listArray $ repeat maxBound

part1, part2 :: Grid -> Maybe Int
part1 = (`solve` [1 .. 3])
part2 = (`solve` [4 .. 10])
