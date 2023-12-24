{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

module Day24 where

import Data.Char (isDigit)
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP

data Coord = Coord {x, y, z :: Double}
  deriving (Eq, Ord, Show)

instance Semigroup Coord where
  Coord x1 y1 z1 <> Coord x2 y2 z2 = Coord (x1 + x2) (y1 + y2) (z1 + z2)

instance Monoid Coord where
  mempty = Coord 0 0 0

mult :: Coord -> Double -> Coord
mult (Coord x y z) k = Coord (x * k) (y * k) (z * k)

data Trajectory = Trajectory {origin, speed :: Coord} deriving (Eq, Ord, Show)

parse :: ByteString -> Maybe [Trajectory]
parse = fmap fst . viaNonEmpty last . readP_to_S (trajectory `sepBy1` char '\n') . decodeUtf8
  where
    trajectory = Trajectory <$> coord <* string " @ " <* skipSpaces <*> coord
    coord = Coord <$> int <* char ',' <* skipSpaces <*> int <* char ',' <* skipSpaces <*> int
    int = Unsafe.read <$> ((<>) <$> option "" (string "-") <*> many1 (satisfy isDigit))

-- https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
intersection2D :: (Coord, Coord) -> (Coord, Coord) -> Maybe Coord
intersection2D (Coord x1 y1 _, Coord x2 y2 _) (Coord x3 y3 _, Coord x4 y4 _)
  | denom == 0 = Nothing
  | otherwise = Just $ Coord px py 0
  where
    denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    a = x1 * y2 - y1 * x2
    b = x3 * y4 - y3 * x4
    px = (a * (x3 - x4) - (x1 - x2) * b) / denom
    py = (a * (y3 - y4) - (y1 - y2) * b) / denom

intersection :: Trajectory -> Trajectory -> Maybe Coord
intersection t1@(Trajectory o1 v1) t2@(Trajectory o2 v2) = do
  inter <- intersection2D (o1, o1 <> v1 `mult` 1000000) (o2, o2 <> v2 `mult` 1000000)
  guard $ isInFuture t1 inter && isInFuture t2 inter
  pure inter

isInFuture :: Trajectory -> Coord -> Bool
isInFuture (Trajectory (Coord x1 y1 _) (Coord vx vy _)) (Coord x2 y2 _) = (x2 - x1) * vx >= 0 || (y2 - y1) * vy >= 0

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (v : xs) = map (v,) xs <> allPairs xs

allIntersections :: [Trajectory] -> [Coord]
allIntersections = mapMaybe (uncurry intersection) . allPairs

inRange :: (Coord, Coord) -> Coord -> Bool
inRange (Coord x1 y1 _, Coord x2 y2 _) (Coord x y _) = x >= x1 && x <= x2 && y >= y1 && y <= y2

solve1 :: Double -> Double -> [Trajectory] -> [Coord]
solve1 a b = filter (inRange (Coord a a 0, Coord b b 0)) . allIntersections

part1 :: [Trajectory] -> Int
part1 = length . solve1 200000000000000 400000000000000
