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

good :: ReadP ()
good = choice [char '.', char '?'] $> ()

bad :: ReadP ()
bad = choice [char '#', char '?'] $> ()

buildParser :: [Int] -> ReadP ()
buildParser l = do
  _ <- many good
  sequenceA_ $ intersperse (many1 good) [count x bad | x <- l]
  _ <- many good <* eof

  return ()

combinations :: Spring -> Int
combinations (s, l) = length $ readP_to_S (buildParser l) s

part1 :: Problem -> Int
part1 = sum . fmap combinations

run1 :: FilePath -> IO Int
run1 f = readFileBS f >>= maybe (fail "parse error") (return . part1) . parse

-- Brute force approach
part2' :: Problem -> Int
part2' = sum . fmap (combinations . bimap (join . intersperse "?" . replicate 5) (join . replicate 5))

part2 :: Problem -> Int
part2 = undefined

run2 :: FilePath -> IO Int
run2 f = readFileBS f >>= maybe (fail "parse error") (return . part2) . parse
