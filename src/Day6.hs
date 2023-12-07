module Day6 where

import Data.Char (isDigit)
import Relude hiding (get)
import Relude.Unsafe (read)
import Text.ParserCombinators.ReadP
import Prelude (readFile)

polyroots :: (Integral a) => a -> a -> (Int, Int)
polyroots b' c' =
  let a = -1.0 :: Double
      b = fromIntegral b'
      c = fromIntegral c'
      sd = sqrt $ b * b - 4 * a * (-c)
   in (ceiling $ (-b - sd) / (2 * a), floor $ (-b + sd) / (2 * a))

solve :: [(Int, Int)] -> Int
solve = product . fmap ((pred . uncurry (-)) . uncurry polyroots)

part1, part2 :: String -> Maybe Int
part1 = fmap solve . runParser parseInput
part2 = part1 . filter (/= ' ')

-- Parsing

runParser :: ReadP a -> String -> Maybe a
runParser p s = case readP_to_S p s of
  [(a, "")] -> Just a
  _ -> Nothing

parseInput :: (Read a) => ReadP [(a, a)]
parseInput = zip <$> parseLine <*> parseLine

parseLine :: (Read a) => ReadP [a]
parseLine = skipMany1 (satisfy (/= ':')) *> char ':' *> skipSpaces *> many1 (satisfy isDigit) `sepBy` many1 (char ' ') <* char '\n' <&> fmap read

-- IO

run1 :: FilePath -> IO (Maybe Int)
run1 file = part1 <$> Prelude.readFile file

run2 :: FilePath -> IO (Maybe Int)
run2 file = part1 <$> Prelude.readFile file
