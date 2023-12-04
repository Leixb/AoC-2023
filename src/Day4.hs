module Day4 where

import Relude
import Relude.Unsafe (read)
import Text.Parsec hiding (spaces)
import Text.Parsec.Text

data Card = Card {num :: Int, winners :: [Int], numbers :: [Int]}
  deriving (Show)

spaces = skipMany $ char ' '

parser :: Parser Card
parser = do
  num <- string "Card" *> spaces *> many1 digit <* char ':' <* spaces
  let parseNumbers = many1 digit `sepEndBy` spaces
  winners <- parseNumbers <* char '|' <* spaces
  have <- parseNumbers

  pure $ Card (read num) (read <$> winners) (read <$> have)

parseInput :: Text -> [Card]
parseInput = either (error . show) id . parse (parser `sepEndBy` newline <* eof) ""

getWinners :: Card -> [Int]
getWinners (Card _ winners numbers) = filter (`elem` winners) numbers

score :: Card -> Int
score = (`div` 2) . (2 ^) . length . getWinners

part1 :: Text -> Int
part1 = sum . fmap score . parseInput

part2 :: Text -> Int
part2 = sum . scorePile (repeat 1) . fmap score2 . parseInput

score2 :: Card -> Int
score2 = length . getWinners

scorePile :: [Int] -> [Int] -> [Int]
scorePile _ [] = []
scorePile (a : as) (s : ss) =
  let (y, ys) = splitAt s as
      newas = ((+ a) <$> y) <> ys
   in a : scorePile newas ss
scorePile [] _ = error "scorePile: impossible"
