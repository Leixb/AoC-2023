module Day23 where

import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as C
import Data.List (maximum)
import qualified Data.Set as S
import Relude

type Coord = (Int, Int)

type Step = (Coord, Int)

parse :: C.ByteString -> UArray Coord Char
parse input = listArray ((0, 0), (h - 1, w - 1)) . C.unpack . C.filter (/= '\n') $ input
  where
    h = C.count '\n' input
    w = C.length $ C.takeWhile (/= '\n') input

start, end :: UArray Coord Char -> Coord
start = const (0, 1)
end = second pred . snd . bounds

neighbors :: UArray Coord Char -> Coord -> [Step]
neighbors maze pos = mapMaybe (toStep maze) . filter (inRange (bounds maze)) $ fmap ($ pos) [first pred, first succ, second pred, second succ]

dfs :: UArray Coord Char -> Coord -> Coord -> Set Coord -> Int -> [Int]
dfs maze pos target visited steps
  | pos == target = [steps]
  | null candidates = []
  | otherwise = maximum $ (\(p, s) -> dfs maze p target (S.insert p visited) (steps + s)) <$> candidates
  where
    candidates = filter ((`S.notMember` visited) . fst) $ neighbors maze pos

toStep :: UArray Coord Char -> Coord -> Maybe Step
toStep maze pos@(y, x)
  | v == '>' = Just ((y, x + 1), 2)
  | v == '<' = Just ((y, x - 1), 2)
  | v == '^' = Just ((y - 1, x), 2)
  | v == 'v' = Just ((y + 1, x), 2)
  | v == '.' = Just (pos, 1)
  | otherwise = Nothing
  where
    v = maze ! pos

part1 :: UArray Coord Char -> Maybe Int
part1 maze = viaNonEmpty head $ dfs maze (start maze) (end maze) S.empty 0
