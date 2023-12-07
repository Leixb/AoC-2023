module Day7 where

import Data.Char
import Relude
import Relude.Unsafe (read)
import Text.ParserCombinators.ReadP

type Card = Int

type Problem = [Bet]

newtype Hand = Hand [Card] deriving (Show, Eq)

data Bet = Bet {hand :: Hand, bet :: Int} deriving (Show, Eq)

-- Part 1

instance Ord Hand where
  compare (Hand h1) (Hand h2) =
    case compare (toPairings h1) (toPairings h2) of
      EQ -> compare h1 h2
      x -> x
    where
      toPairings = sortOn Down . fmap length . group . sort

part1 :: Problem -> Int
part1 = sum . zipWith (*) [1 ..] . fmap bet . sortOn hand

-- Parsing

parseHands :: ReadP Problem
parseHands = ((Bet . Hand <$> many1 parseCard) <* char ' ' <*> (read <$> munch1 isDigit)) `sepBy` char '\n' <* char '\n' <* eof
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
