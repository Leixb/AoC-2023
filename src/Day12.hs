module Day12 where

import Data.Char (isDigit)
import Data.List (dropWhileEnd)
import Relude hiding (get, many)
import Relude.Unsafe (read)
import Text.ParserCombinators.ReadP

data Status = Good | Bad | Unknown deriving (Eq, Show)

type Spring = (String, [Int])

type Problem = [Spring]

parseStatus :: ReadP Char
parseStatus = choice $ char <$> ".#?"

parseSpring :: ReadP Spring
parseSpring = do
  status <- many1 parseStatus <* char ' '
  listFailed <- (read <$> munch1 isDigit) `sepBy` char ','
  return (status, listFailed)

parseProblem :: ReadP Problem
parseProblem = parseSpring `sepBy` char '\n'

parse :: ByteString -> Maybe Problem
parse = fmap fst . viaNonEmpty last . readP_to_S parseProblem . decodeUtf8

good :: ReadP Status
good = choice [char '.', char '?'] $> Good

bad :: ReadP Status
bad = choice [char '#', char '?'] $> Bad

buildParser :: [Int] -> ReadP [Status]
buildParser l = do
  f <- many good
  m <- sequenceA $ intersperse (many1 good) [count x bad | x <- l]
  e <- many good <* eof

  return $ concat [f, join m, e]

combinations :: Spring -> Int
combinations (s, l) = length $ readP_to_S (buildParser l) s

part1 :: Problem -> Int
part1 = sum . fmap combinations

run1 :: FilePath -> IO Int
run1 f = readFileBS f >>= maybe (fail "parse error") (return . part1) . parse
