{-# LANGUAGE TupleSections #-}

module Day22 (part1, part2, parse) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Char (isDigit)
import Data.Foldable (Foldable (maximum, minimum))
import Data.List (nub)
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP

type Coord = ((Int, Int), Int)

x, y, z :: Coord -> Int
x = fst . fst
y = snd . fst
z = snd

type Brick = (Coord, Coord)

parse :: ByteString -> Maybe [Brick]
parse = fmap fst . viaNonEmpty last . readP_to_S (brick `sepBy` char '\n') . decodeUtf8
  where
    pair p sep = (,) <$> p <* char sep <*> p
    brick = pair coord '~'
    coord = (,) <$> pair int ',' <* char ',' <*> int
    int = Unsafe.read <$> munch1 isDigit

-- x and y coordinates of points in the brick and height.
type CoordListWithHeight = ([(Int, Int)], Int)

-- fst, brick id; snd, ids of bricks that support it
type Dependency = (Int, [Int])

brickToCoordList :: Brick -> CoordListWithHeight
brickToCoordList (((x1, y1), z1), ((x2, y2), z2)) = ([(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]], z2 - z1 + 1)

push :: (STArray s (Int, Int) (Int, Int), [Dependency]) -> (Int, CoordListWithHeight) -> ST s (STArray s (Int, Int) (Int, Int), [Dependency])
push (grid, supports) (n, (coords, h)) = do
  gridValues <- traverse (readArray grid) coords
  let height = maximum $ fst <$> gridValues
      supporters = nub . sort $ snd <$> filter ((== height) . fst) gridValues
  traverse_ (flip (writeArray grid) (height + h, n)) coords
  pure (grid, (n, supporters) : supports)

getNeeded :: [(Int, [Int])] -> [Int]
getNeeded = concatMap snd . filter ((== 1) . length . snd)

findDeps :: [Brick] -> [Dependency]
findDeps p = runST $ newArray ((minX, minY), (maxX, maxY)) (0, -1) >>= fmap snd . flip (foldM push) queue . (,[])
  where
    sorted = sortOn (z . fst) p
    ((minX, maxX), (minY, maxY)) = bimap (minimum &&& maximum) (minimum &&& maximum) . (fmap x &&& fmap y) . fmap fst $ sorted
    queue = zip [1 ..] $ brickToCoordList <$> sorted

part1, part2 :: [Brick] -> Int
part1 p = length p - length needed
  where
    needed = filter (/= -1) . nub . sort . getNeeded $ findDeps p
part2 p = getSum $ mconcat [calcFalls m [n] | n <- [1 .. length p]]
  where
    m = filter ((/= [-1]) . snd) . sort $ findDeps p

calcFalls :: [Dependency] -> [Int] -> Sum Int
calcFalls [] _ = mempty
calcFalls ((a, b) : xs) fallen
  | all (`elem` fallen) b = 1 <> calcFalls xs (a : fallen)
  | otherwise = calcFalls xs fallen
