{-# LANGUAGE DeriveAnyClass #-}

module Day3 where

import Data.List (groupBy)
import qualified Data.Text as T
import Relude hiding (many, some, (<|>))
import Relude.Unsafe (read)
import Text.Parsec hiding (Empty)
import Text.Parsec.Text

data Cell = Digit Char | Empty | Symbol deriving (Show, Eq)

type Schematic = [[Cell]]

type Coords = (Int, Int)

type CellA = (Cell, Coords)

type SchematicA = [[CellA]]

type Number = (Int, [Coords])

parseSchematic :: Parser Schematic
parseSchematic = many (many parseCell <* endOfLine) <* eof
  where
    parseCell = Digit <$> digit <|> Empty <$ char '.' <|> Symbol <$ noneOf "\n"

addCoords :: Schematic -> SchematicA
addCoords = zipWith (\y -> zipWith (\x c -> (c, (x, y))) [0 ..]) [0 ..]

is :: Cell -> Cell -> Bool
Digit _ `is` Digit _ = True
Empty `is` Empty = True
Symbol `is` Symbol = True
_ `is` _ = False

isDigit :: Cell -> Bool
isDigit (Digit _) = True
isDigit _ = False

isSymbol :: Cell -> Bool
isSymbol Symbol = True
isSymbol _ = False

processLines :: SchematicA -> Int
processLines s =
  let symbols = fmap snd . filter (isSymbol . fst) . concat $ s -- coordinates of symbols
      groups = mapMaybe processGroup $ groupBy (\x y -> fst x `is` fst y) =<< s
   in sum . fmap fst $ filter (anyAnyNeighbours symbols . snd) (trace (show groups) groups)

getDigit :: Cell -> Maybe Char
getDigit (Digit d) = Just d
getDigit _ = Nothing

anyAnyNeighbours :: [Coords] -> [Coords] -> Bool
anyAnyNeighbours symbols = any (anyNeighbours symbols)

anyNeighbours :: [Coords] -> Coords -> Bool
anyNeighbours symbols p = any (`elem` symbols) (neighbours p)

neighbours :: Coords -> [Coords]
neighbours (x, y) =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

processGroup :: [CellA] -> Maybe Number
processGroup l = do
  num <- readMaybe $ l >>= maybeToList . getDigit . fst :: Maybe Int
  let coords = snd <$> l

  pure (num, coords)

part1 :: Text -> Int
part1 = processLines . addCoords . fromRight [] . parse parseSchematic ""
