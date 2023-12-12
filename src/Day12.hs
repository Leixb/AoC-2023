module Day12 where

import Data.Array
import Data.Char (isDigit)
import Data.List ((!!))
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

part1, part2 :: Problem -> Int
part1 = sum . fmap combinations
part2 = sum . fmap (combinations' . toSpring' . bimap (join . intersperse "?" . replicate 5) (join . replicate 5))

run1, run2 :: FilePath -> IO Int
run1 f = readFileBS f >>= maybe (fail "parse error") (return . part1) . parse
run2 f = readFileBS f >>= maybe (fail "parse error") (return . part2) . parse

type Spring' = ([Status], [Int])

type Problem' = [Spring']

toSpring' :: Spring -> Spring'
toSpring' (s, l) = (fmap toStatus s, l)
  where
    toStatus :: Char -> Status
    toStatus '.' = Good
    toStatus '#' = Bad
    toStatus '?' = Unknown
    toStatus _ = error "impossible"

isGood :: Status -> Bool
isGood Bad = False
isGood _ = True

isBad :: Status -> Bool
isBad Good = False
isBad _ = True

combinations' :: Spring' -> Int
combinations' (s, l) = t ! (0, 0)
  where
    n = length s
    m = length l

    t = listArray ((0, 0), (n, m)) [go i j | i <- [0 .. n], j <- [0 .. m]]

    go :: Int -> Int -> Int
    go n' m'
      | n' >= n = if m' >= m then 1 else 0
      | v == Unknown = tGood + tBad
      | v == Good = tGood
      | v == Bad = tBad
      | otherwise = error "impossible"
      where
        v = s !! n'
        x = l !! m'

        ss = drop n' s

        (bads, rest) = splitAt x ss
        badsDelimited = maybe True isGood (viaNonEmpty head rest)
        off = if null rest then 0 else 1

        tGood = t ! (n' + 1, m')

        tBad =
          if m' + 1 <= m && length bads == x && all isBad bads && badsDelimited
            then t ! (n' + x + off, m' + 1)
            else 0
