{-# LANGUAGE DeriveAnyClass #-}

module Day3 where

import Data.List (groupBy, (!!))
import Data.Map (Map, fromList, lookup)
import qualified Data.Text as T
import Relude hiding (fromList, many, some, (<|>))
import qualified Relude as Data.List.NonEmpty
import Relude.Unsafe (read)
import Text.Parsec hiding (Empty)
import Text.Parsec.Text

data Cell = Digit Char | Empty | Symbol | Gear deriving (Show, Eq)

type Schematic = [[Cell]]

type Coords = (Int, Int)

type CellA = (Cell, Coords)

type SchematicA = [[CellA]]

type Number = (Int, [Coords])

parseSchematic :: Parser Schematic
parseSchematic = many (many parseCell <* endOfLine) <* eof
  where
    parseCell = Digit <$> digit <|> Empty <$ char '.' <|> Gear <$ char '*' <|> Symbol <$ noneOf "\n"

addCoords :: Schematic -> SchematicA
addCoords = zipWith (\y -> zipWith (\x c -> (c, (x, y))) [0 ..]) [0 ..]

is :: Cell -> Cell -> Bool
Digit _ `is` Digit _ = True
Empty `is` Empty = True
Symbol `is` Symbol = True
Gear `is` Symbol = True
Symbol `is` Gear = True
Gear `is` Gear = True
_ `is` _ = False

isDigit :: Cell -> Bool
isDigit (Digit _) = True
isDigit _ = False

isSymbol :: Cell -> Bool
isSymbol Symbol = True
isSymbol Gear = True
isSymbol _ = False

isGear :: Cell -> Bool
isGear Gear = True
isGear _ = False

processLines :: SchematicA -> Int
processLines s =
  let symbols = fmap snd . filter (isSymbol . fst) . concat $ s -- coordinates of symbols
      groups = mapMaybe processGroup $ groupBy (\x y -> fst x `is` fst y) =<< s
   in sum . fmap fst $ filter (anyAnyNeighbours symbols . snd) groups

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

part1, part2 :: Text -> Int
part1 = processLines . addCoords . fromRight [] . parse parseSchematic ""
part2 = processLines2 . addCoords . fromRight [] . parse parseSchematic ""

processLines2 :: SchematicA -> Int
processLines2 s =
  let gears = fmap snd . filter (isGear . fst) . concat $ s -- coordinates of gears
      groups = mapMaybe processGroup $ groupBy (\x y -> fst x `is` fst y) =<< s
      nums = fst <$> groups
      numberedGroups = zip (snd <$> groups) [0 ..]
      coordToIndex = fmap (\(n, i) -> [(c, i) | c <- n]) numberedGroups
      coordToIndexMap = fromList . concat $ coordToIndex
      gearsNeighbours = gearNeightbours coordToIndexMap <$> gears
      valid = filter ((== 2) . length) gearsNeighbours
   in sum . fmap (product . fmap (nums !!)) $ valid

gearNeightbours :: Map Coords Int -> Coords -> [Int]
gearNeightbours m p =
  mapMaybe (viaNonEmpty head) . Data.List.NonEmpty.group . sort $ mapMaybe (`lookup` m) (neighbours p)
