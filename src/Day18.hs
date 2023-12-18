module Day18 where

import Data.ByteString.Char8 (unpack)
import Data.Char (isDigit, isHexDigit)
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP

data Dir = R | D | L | U deriving (Show, Eq)

type Pos = (Int, Int)

data Action = Action Dir Int deriving (Show, Eq)

parse :: ByteString -> Maybe [(Action, Action)]
parse = fmap fst . viaNonEmpty last . readP_to_S (sepBy1 parseAction (char '\n') <* char '\n' <* eof) . unpack
  where
    parseAction = do
      dir <- choice [U <$ char 'U', D <$ char 'D', L <$ char 'L', R <$ char 'R'] <* char ' '
      x <- Unsafe.read <$> munch1 isDigit <* char ' '
      y <- char '(' *> char '#' *> (Unsafe.read . ("0x" ++) <$> count 5 (satisfy isHexDigit))
      dir' <- choice [R <$ char '0', D <$ char '1', L <$ char '2', U <$ char '3'] <* char ')'
      return (Action dir x, Action dir' y)

vertices :: [Action] -> [Pos]
vertices = scanl' (flip step) origin
  where
    step (Action U n) = first $ subtract n
    step (Action D n) = first (+ n)
    step (Action L n) = second $ subtract n
    step (Action R n) = second (+ n)

origin :: Pos
origin = (0, 0)

area, perimeter, solve :: [Action] -> Int
area a = (`div` 2) . abs . sum $ zipWith (-) x y
  where
    (p, rp) = (origin :) &&& (++ [origin]) $ vertices a
    x = zipWith (*) (fst <$> p) (snd <$> rp)
    y = zipWith (*) (snd <$> p) (fst <$> rp)
perimeter = sum . fmap (\(Action _ n) -> n)
solve = area &&& (`div` 2) . perimeter >>> uncurry (+) >>> succ

part1, part2 :: [(Action, Action)] -> Int
part1 = solve . fmap fst
part2 = solve . fmap snd
