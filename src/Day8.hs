module Day8 where

import Data.Char (isAlpha)
import qualified Data.Map as M
import Relude
import Text.ParserCombinators.ReadP

type Side a = (a, a) -> a

type Node = String

type Problem = (Map Node (Node, Node), [Side Node])

parse :: ReadP Problem
parse = do
  turns <- many1 (choice [char 'L' $> fst, char 'R' $> snd]) <* char '\n' <* char '\n'

  nodes <- flip sepBy (char '\n') $ do
    name <- munch1 isAlpha <* string " = "
    value <- between (char '(') (char ')') $ (,) <$> munch1 isAlpha <* string ", " <*> munch1 isAlpha
    pure (name, value)
  _ <- char '\n' <* eof

  return (M.fromList nodes, cycle turns)

start, end :: Node
start = "AAA"
end = "ZZZ"

part1 :: Problem -> Int
part1 (m, t) = length . takeWhile (/= end) $ scanl' (\x f -> f $ m M.! x) start t
