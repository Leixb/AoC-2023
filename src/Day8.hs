module Day8 where

import Data.Char (isAlphaNum)
import qualified Data.Map as M
import Relude
import Text.ParserCombinators.ReadP

type Side a = (a, a) -> a

type Node = String

type NodeMap = Map Node (Node, Node)

type Problem = (NodeMap, [Side Node])

parse :: ReadP Problem
parse = do
  turns <- many1 (choice [char 'L' $> fst, char 'R' $> snd]) <* char '\n' <* char '\n'

  nodes <- flip sepBy (char '\n') $ do
    name <- munch1 isAlphaNum <* string " = "
    value <- between (char '(') (char ')') $ (,) <$> munch1 isAlphaNum <* string ", " <*> munch1 isAlphaNum
    pure (name, value)
  _ <- char '\n' <* eof

  return (M.fromList nodes, cycle turns)

start, end :: Node
start = "AAA"
end = "ZZZ"

step :: NodeMap -> Node -> Side Node -> Node
step m n f = case M.lookup n m of
  Nothing -> error "Invalid node"
  Just fork -> f fork

part1 :: Problem -> Int
part1 (m, t) = length . takeWhile (/= end) $ scanl' (step m) start t

-- | Part 2
--
-- Doing it naively is too slow, we need to find the pattern of when
-- the ghosts are on end nodes. Taking the LCM of the lengths of the cycles
-- of each ghost will give us the answer.
isStart, isEnd :: Node -> Bool
isStart = (Just 'A' ==) . viaNonEmpty last
isEnd = (Just 'Z' ==) . viaNonEmpty last

getStarts :: Problem -> [Node]
getStarts (m, _) = M.keys $ M.filterWithKey (\k _ -> isStart k) m

part2 :: Problem -> Int
part2 (m, t) =
  let simulStep :: [Node] -> Side Node -> [Node]
      simulStep ns f = fmap (flip (step m) f) ns
      starts = getStarts (m, t)
      l = ghostSteps . scanl' simulStep starts $ t
   in foldl' lcm 1 [computeOne $ filter ((== i) . snd) l | i <- [1 .. (length starts)]]

endIndices :: Int -> [Node] -> [(Int, Int)]
endIndices n l = catMaybes $ zipWith (\i x -> if isEnd x then Just (n, i) else Nothing) [1 ..] l

ghostSteps :: [[Node]] -> [(Int, Int)]
ghostSteps l = concat $ zipWith endIndices [1 ..] l

computeOne :: [(Int, Int)] -> Int
computeOne ((a, _) : (b, _) : _) = b - a
computeOne _ = error "Invalid input"
