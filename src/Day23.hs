{-# LANGUAGE TupleSections #-}

module Day23 where

import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as C
import Data.List (maximum, nub)
import qualified Data.Map as M
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

neighbors :: UArray Coord Char -> Coord -> [Coord]
neighbors maze pos = filter (inRange (bounds maze)) $ fmap ($ pos) [first pred, first succ, second pred, second succ]

dfs :: UArray Coord Char -> Coord -> Coord -> Set Coord -> Int -> [Int]
dfs maze pos target visited steps
    | pos == target = [steps]
    | null candidates = []
    | otherwise = maximum $ (\(p, s) -> dfs maze p target (S.insert p visited) (steps + s)) <$> candidates
  where
    candidates = filter ((`S.notMember` visited) . fst) . mapMaybe (toStep maze) $ neighbors maze pos

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

followPath :: UArray Coord Char -> Coord -> Coord -> Maybe (Coord, Int)
followPath maze prev pos = case candidates of
    [] -> if pos == end maze then Just (pos, 1) else Nothing
    [next] -> second succ <$> followPath maze pos next
    _ -> Just (pos, 1)
  where
    candidates = filter (uncurry (&&) . ((/= prev) &&& ((/= '#') . (maze !)))) $ neighbors maze pos

process :: UArray Coord Char -> [Coord] -> Set Coord -> [(Coord, (Coord, Int))]
process _ [] _ = []
process maze (p : ps) nodes
    | p `S.member` nodes = process maze ps nodes
    | otherwise = next ++ process maze (ps ++ (fst <$> reachable)) nodes'
  where
    next = fmap (p,) <$> reachable
    reachable = catMaybes $ followPath maze p <$> candidates
    candidates = filter (uncurry (&&) . ((`S.notMember` nodes') &&& ((/= '#') . (maze !)))) . neighbors maze $ p
    nodes' = S.insert p nodes

part2 :: UArray Coord Char -> Int
part2 maze =
    let
        proc = process maze [start maze] S.empty
        proc' = (\(p, (c, d)) -> (p, c, d)) <$> proc
        adj = M.map (nub . sort) $ toAdjacencyList proc'
     in
        maximum $ dfs' adj (start maze) (end maze) S.empty 0

toAdjacencyList :: [(Coord, Coord, Int)] -> M.Map Coord [(Coord, Int)]
toAdjacencyList = foldl' (\m' (p, c, d) -> M.insertWith (++) p [(c, d)] (M.insertWith (++) c [(p, d)] m')) M.empty

dfs' :: M.Map Coord [(Coord, Int)] -> Coord -> Coord -> Set Coord -> Int -> [Int]
dfs' graph pos target visited steps
    | pos == target = [steps]
    | null candidates = []
    | otherwise = candidates >>= (\(p, s) -> dfs' graph p target (S.insert p visited) (steps + s))
  where
    candidates = filter ((`S.notMember` visited) . fst) $ graph M.! pos
