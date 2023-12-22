module Day22 (part1, part2, parse) where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Char (isDigit)
import Data.Foldable (Foldable (maximum))
import Data.List (nub)
import qualified Data.Map as M
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP

type Coord = ((Int, Int), Int)

x, y, z :: Coord -> Int
x = fst . fst
y = snd . fst
z = snd

type Brick = (Coord, Coord)

type Problem = [Brick]

parse :: ByteString -> Maybe Problem
parse = fmap fst . viaNonEmpty last . readP_to_S (brick `sepBy` char '\n') . decodeUtf8
  where
    pair p sep = (,) <$> p <* char sep <*> p
    brick = pair coord '~'
    coord = (,) <$> pair int ',' <* char ',' <*> int
    int = Unsafe.read <$> munch1 isDigit

brickToCoordList :: Brick -> ([(Int, Int)], Int)
brickToCoordList (((x1, y1), z1), ((x2, y2), z2)) = ([(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]], z2 - z1 + 1)

push :: (STArray s (Int, Int) (Int, Int), M.Map Int [Int]) -> (Int, ([(Int, Int)], Int)) -> ST s (STArray s (Int, Int) (Int, Int), M.Map Int [Int])
push (grid, supports) (n, (coords, h)) = do
  gridValues <- traverse (readArray grid) coords
  let height = maximum $ fst <$> gridValues
      supporters = nub . sort $ snd <$> filter ((== height) . fst) gridValues
      supports' = M.insert n supporters supports
  traverse_ (\p -> writeArray grid p (height + h, n)) coords
  pure (grid, supports')

process :: [(Int, ([(Int, Int)], Int))] -> Array (Int, Int) (Int, Int) -> M.Map Int [Int] -> M.Map Int [Int]
process queue grid supports = runST $ do
  grid' <- thaw grid
  snd <$> foldM push (grid', supports) queue

getNeeded :: [(Int, [Int])] -> [Int]
getNeeded = concatMap snd . filter ((== 1) . length . snd)

findDeps :: Problem -> Map Int [Int]
findDeps p = process queue grid M.empty
  where
    sorted = sortOn (z . fst) p
    mx = maximum (x . fst <$> sorted)
    my = maximum (y . fst <$> sorted)
    grid = listArray ((0, 0), (my, mx)) $ repeat (0, -1)
    queue = zip [1 ..] $ brickToCoordList <$> sorted

part1, part2 :: Problem -> Int
part1 p = length p - length needed
  where
    needed = filter (/= -1) . nub . sort . getNeeded . M.toList $ findDeps p
part2 p = getSum $ mconcat [calcFalls m [n] | n <- [1 .. length p]]
  where
    m = filter ((/= [-1]) . snd) . M.toList $ findDeps p

calcFalls :: [(Int, [Int])] -> [Int] -> Sum Int
calcFalls [] _ = mempty
calcFalls ((a, b) : xs) fallen
  | all (`elem` fallen) b = 1 <> calcFalls xs (a : fallen)
  | otherwise = calcFalls xs fallen
