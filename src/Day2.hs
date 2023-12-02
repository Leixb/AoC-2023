{-# LANGUAGE LambdaCase #-}

module Day2 where

import Data.Foldable (Foldable (maximum))
import Relude
import Relude.Unsafe (read)
import Text.Parsec
import Text.Parsec.Text

data Game = Game {gameID :: Int, rounds :: [Round]}
  deriving (Show, Eq)

type Round = [(Int, Color)]

data Color = Red | Green | Blue
  deriving (Show, Eq)

-- Parse input into structure

parseGames :: Parser [Game]
parseGames = parseGame `sepEndBy` char '\n'

parseGame :: Parser Game
parseGame = do
  gID <- string "Game " *> many1 digit <* string ": "
  rounds <- parseRound `sepBy` string "; "
  return $ Game (read gID) rounds

parseRound :: Parser Round
parseRound = parseColor `sepBy` string ", "

parseColor :: Parser (Int, Color)
parseColor = do
  n <- many1 digit <* char ' '
  c <-
    many1 letter >>= \case
      "red" -> return Red
      "green" -> return Green
      "blue" -> return Blue
      _ -> fail "Invalid color"
  return (read n, c)

-- Part 1

validColor :: (Int, Color) -> Bool
validColor (n, Red) = n <= 12
validColor (n, Green) = n <= 13
validColor (n, Blue) = n <= 14

validGame :: Game -> Bool
validGame = all (all validColor) . rounds

part1 :: Text -> Int
part1 input = case parse parseGames "" input of
  Left err -> error $ show err
  Right gs -> sum $ gameID <$> filter validGame gs

-- Part 2

part2 :: Text -> Int
part2 input = case parse parseGames "" input of
  Left err -> error $ show err
  Right gs -> sum $ solve2 <$> gs

minRound :: Color -> [Round] -> Int
minRound c rounds = maximum . fmap fst $ filter ((== c) . snd) =<< rounds

solve2 :: Game -> Int
solve2 = product . ((minRound <$> [Red, Green, Blue]) <*>) . pure . rounds
