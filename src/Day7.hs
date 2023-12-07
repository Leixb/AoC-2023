{-# LANGUAGE DeriveFunctor #-}

module Day7 where

import Data.Char
import Data.List (groupBy, maximumBy)
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP

data Card = Num Int | Joker Int deriving (Show, Eq)

type Problem = [Bet [Card]]

data Bet a = Bet {hand :: a, bet :: Int} deriving (Show, Eq, Functor)

instance Ord Card where
  compare (Num x) (Num y) = compare x y
  compare Num {} Joker {} = GT -- Joker is weakest
  compare Joker {} Num {} = LT
  compare Joker {} Joker {} = EQ

toInt :: Card -> Int
toInt (Num x) = x
toInt (Joker x) = x

-- Part 1

rankCards :: [Card] -> [Card] -> Ordering
rankCards h1 h2 = case compare (fst <$> toPairings' h1) (fst <$> toPairings' h2) of
  EQ -> compare h1 h2
  x -> x

rankCardsJoker :: [Card] -> [Card] -> Ordering
rankCardsJoker h1 h2 = case compare (fst <$> toPairings (toInt <$> h1)) (fst <$> toPairings (toInt <$> h2)) of
  EQ -> compare h1 h2
  x -> x

toPairings :: (Ord a) => [a] -> [(Int, a)]
toPairings = sortOn Down . fmap (length &&& Unsafe.head) . group . sort

toPairings' :: [Card] -> [(Int, Card)]
toPairings' = sortOn Down . fmap (length &&& Unsafe.head) . groupBy (\a b -> toInt a == toInt b) . sortOn toInt

part1 :: Problem -> Int
part1 = sum . zipWith (*) [1 ..] . fmap bet . sortBy (rankCards `on` hand)

-- Part 2

validJokerValues :: [Int]
validJokerValues = [2 .. 10] <> [12, 13, 14]

jokerId :: Int
jokerId = 11

convertJokers :: Problem -> Problem
convertJokers = fmap . fmap . fmap $ convertJoker

convertJoker :: Card -> Card
convertJoker (Num x)
  | x == jokerId = Joker x
  | otherwise = Num x
convertJoker x = x

replaceJoker :: Int -> Card -> Card
replaceJoker n (Joker _) = Joker n
replaceJoker _ x = x

possibleHands :: [Card] -> [[Card]]
possibleHands l = do
  v <- validJokerValues
  pure $ replaceJoker v <$> l

bestHand :: [Card] -> [Card]
bestHand = maximumBy rankCardsJoker . possibleHands

getBest :: Problem -> Problem
getBest = fmap (fmap bestHand)

part2 :: Problem -> Int
part2 = part1 . getBest . convertJokers

run1, run2 :: FilePath -> IO (Maybe Int)
run1 file = fmap part1 . runParser parseHands . decodeUtf8 <$> readFileBS file
run2 file = fmap part2 . runParser parseHands . decodeUtf8 <$> readFileBS file

-- Parsing

parseHands :: ReadP Problem
parseHands = ((Bet <$> parseHand) <* char ' ' <*> (Unsafe.read <$> munch1 isDigit)) `sepBy` char '\n' <* char '\n' <* eof

parseHand :: ReadP [Card]
parseHand = many1 parseCard
  where
    parseCard =
      asum
        [ Num . subtract (ord '0') . ord <$> satisfy isDigit,
          Num 10 <$ char 'T',
          Num 11 <$ char 'J',
          Num 12 <$ char 'Q',
          Num 13 <$ char 'K',
          Num 14 <$ char 'A'
        ]

runParser :: ReadP a -> String -> Maybe a
runParser p s = case readP_to_S p s of
  [(a, "")] -> Just a
  _ -> Nothing
