{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day17 where

import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt)
import Data.Heap hiding (filter)
import qualified Data.Heap as H hiding (filter)
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
doMoveN g d n = foldl' (>=>) pure $ replicate n (doMove g d)

doMoves :: Grid -> S -> Dir -> [S]
doMoves g s d = mapMaybe (flip (doMoveN g d) s) [1 .. 3]

allMoves :: Grid -> S -> [S]
allMoves g s@(_, _, prev) = nextDir prev >>= doMoves g s

solve :: Grid -> UArray (Pos, Dir) Int -> Pos -> MinHeap S -> Maybe Int
solve g distances target h = do
  ((acc, pos, dir), h') <- H.view h

  if pos == target
    then pure acc
    else do
      let moves = allMoves g (acc, pos, dir)
          -- distances' = if acc <= distances ! (pos, dir) then distances // [((pos, dir), acc)] else distances
          moves' = filter (\(acc, p, d) -> acc < distances ! (p, d)) moves
          distances' = foldl' (\dist (acc, p, d) -> dist // [((p, d), acc)]) distances moves'
          h'' = foldl' (flip H.insert) h' moves'
      solve g distances' target h''

part1 :: Grid -> Maybe Int
part1 g =
  let (low, hi) = bounds g
   in solve g (emptyGrid ((low, minBound), (hi, maxBound))) hi (H.singleton (0, (0, 0), U))

emptyGrid :: ((Pos, Dir), (Pos, Dir)) -> UArray (Pos, Dir) Int
emptyGrid = flip listArray (repeat maxBound)
