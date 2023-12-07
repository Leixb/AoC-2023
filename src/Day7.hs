module Day7 where

import Data.Char
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP

type Card = Int

type Problem = [Bet]

data Bet = Bet {hand :: [Card], bet :: Int} deriving (Show, Eq)

-- Part 1

rankCards :: [Card] -> [Card] -> Ordering
rankCards h1 h2 = case compare (fst <$> toPairings h1) (fst <$> toPairings h2) of
  EQ -> compare h1 h2
  x -> x

toPairings :: [Card] -> [(Int, Card)]
toPairings = sortOn Down . fmap (length &&& Unsafe.head) . group . sort

part1 :: Problem -> Int
part1 = sum . zipWith (*) [1 ..] . fmap bet . sortBy (rankCards `on` hand)

run1 :: FilePath -> IO (Maybe Int)
run1 file = fmap part1 . runParser parseHands . decodeUtf8 <$> readFileBS file

-- Parsing

parseHands :: ReadP Problem
parseHands = ((Bet <$> parseHand) <* char ' ' <*> (Unsafe.read <$> munch1 isDigit)) `sepBy` char '\n' <* char '\n' <* eof

parseHand :: ReadP [Card]
parseHand = many1 parseCard
  where
    parseCard =
      asum
        [ subtract (ord '0') . ord <$> satisfy isDigit,
          10 <$ char 'T',
          11 <$ char 'J',
          12 <$ char 'Q',
          13 <$ char 'K',
          14 <$ char 'A'
        ]

runParser :: ReadP a -> String -> Maybe a
runParser p s = case readP_to_S p s of
  [(a, "")] -> Just a
  _ -> Nothing
