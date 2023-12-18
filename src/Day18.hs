{-# LANGUAGE OverloadedStrings #-}

module Day18 where

import Control.Monad.RWS
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.ByteString.Char8 (unpack)
import Data.Char (isDigit, isHexDigit)
import Data.List (maximum, minimum)
import Data.Set (Set)
import qualified Data.Set as Set
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP hiding (get)

data Dir = U | D | L | R deriving (Show, Eq)

type Pos = (Int, Int)

data Action = Action Dir Int Int deriving (Show, Eq)

type Grid = UArray Pos Bool

parse :: ByteString -> Maybe [Action]
parse = fmap fst . viaNonEmpty last . readP_to_S (sepBy1 parseAction (char '\n') <* char '\n' <* eof) . unpack

parseAction :: ReadP Action
parseAction = do
  dir <- choice [U <$ char 'U', D <$ char 'D', L <$ char 'L', R <$ char 'R'] <* char ' '
  x <- Unsafe.read <$> munch1 isDigit <* char ' '
  y <- between (char '(' <* char '#') (char ')') (Unsafe.read . ("0x" ++) <$> munch1 isHexDigit)
  return $ Action dir x y

step :: Dir -> Pos -> Pos
step U = first pred
step D = first succ
step L = second pred
step R = second succ

move :: Action -> RWS () (Set Pos) Pos ()
move (Action d s _) = do
  pos <- get
  let steps = take (s + 1) $ iterate (step d) pos
  tell $ Set.fromList steps
  put $ Unsafe.last steps

border :: [Action] -> (Grid, Int)
border acts = toGrid &&& Set.size $ snd $ execRWS (traverse_ move acts) () (0, 0)

toGrid :: Set Pos -> Grid
toGrid s = runSTUArray $ do
  arr <- newArray ((mny, mnx), (my, mx)) False
  forM_ (Set.toList s) $ \p -> writeArray arr p True
  return arr
  where
    ls = Set.toList s
    mx = succ . maximum . fmap snd $ ls
    my = succ . maximum . fmap fst $ ls
    mnx = pred . minimum . fmap snd $ ls
    mny = pred . minimum . fmap fst $ ls

draw :: Grid -> String
draw s = toString $ unlines $ map (toText . map f) $ [[(y, x) | x <- [mnx .. mx]] | y <- [mny .. my]]
  where
    f p = if s ! p then '#' else '.'
    ((mny, mnx), (my, mx)) = bounds s

findStartingPoint :: Grid -> Pos
findStartingPoint = fst . bounds

type Bounds = (Pos, Pos)

neighbors :: Bounds -> Pos -> [Pos]
neighbors b = filter (inRange b) . flap [first pred, first succ, second pred, second succ]

flodFill :: Grid -> Int
flodFill g' = runST $ do
  g <- thaw g'
  fill g $ Set.singleton $ findStartingPoint g'
  where
    b = bounds g'
    fill :: STUArray s Pos Bool -> Set Pos -> ST s Int
    fill g q = do
      if Set.null q
        then return 0
        else do
          let (p, ps) = Set.deleteFindMin q
          curr <- readArray g p
          if curr
            then fill g ps
            else do
              writeArray g p True
              ns <- filterM (fmap not . readArray g) $ neighbors b p
              n <- fill g (ps <> Set.fromList ns)
              pure $ n + 1

countFilled :: Grid -> Int
countFilled = length . filter not . elems

part1 :: [Action] -> Int
part1 acts =
  let (bord, _) = border acts
      ((mny, mnx), (my, mx)) = bounds bord
      totalArea = (my - mny + 1) * (mx - mnx + 1)
      n = flodFill bord
   in totalArea - n
