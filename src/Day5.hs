{-# LANGUAGE DeriveFunctor #-}

module Day5 where

import Control.Applicative
import Data.List (minimum)
import Relude
import Relude.Unsafe (read)
import Text.Parsec
import Text.Parsec.Text

-- Binary search tree, where each node is an interval
-- It is not an interval tree, since there is no overlapping intervals
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Functor)

type Problem = ([IntervalTree Int], [Int])

data Interval a = Interval {start :: a, end :: a, delta :: a} deriving (Show, Eq, Ord, Functor)

type IntervalTree a = Tree (Interval a)

type Range a = (a, a)

-- Tree Operations

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
  | x < y = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

buildTree :: (Ord a) => [Interval a] -> Tree (Interval a)
buildTree = foldl' (flip insert) Leaf

getDelta :: (Num a, Ord a) => Tree (Interval a) -> a -> a
getDelta Leaf _ = 0
getDelta (Node (Interval s e d) left right) n
  | n >= s && n <= e = d
  | n < s = getDelta left n
  | otherwise = getDelta right n

composeTrees :: (Monoid a, Num a, Ord a) => [Tree (Interval a)] -> a -> a
composeTrees trees n = foldl' (\acc t -> acc <> getDelta t acc) n trees

-- Part 1

part1 :: Problem -> Int
part1 (trees, seeds) =
  let f = withSum (composeTrees $ toSumTree <$> trees)
   in minimum $ f <$> seeds

toSumTree :: IntervalTree a -> IntervalTree (Sum a)
toSumTree = fmap (fmap Sum)

withSum :: (Sum a -> Sum a) -> a -> a
withSum f = getSum . f . Sum

-- Part 2

-- We use the same Tree structure, but instead of using a single value, we use
-- ranges. Querying the tree will give us a list that splits our initial range
-- into subranges with the delta operation applied (see getSubIntervals).
--
-- We use the List Monad to chain the operations together.

part2 :: Problem -> Int
part2 (trees, seeds) = minimum . fmap (getSum . fst) $ pp
  where
    r = seedsToRanges $ Sum <$> seeds
    ft = fmap (getSubRanges . toSumTree) trees
    ct = foldl' (>=>) pure ft
    pp = r >>= ct

toRange :: (Semigroup a) => Interval a -> Range a
toRange (Interval s e d) = (s <> d, e <> d)

getSubRanges :: (Semigroup a, Num a, Ord a, Show a) => Tree (Interval a) -> Range a -> [Range a]
getSubRanges = (fmap toRange .) . getSubIntervals

getSubIntervals :: (Num a, Ord a, Show a) => Tree (Interval a) -> Range a -> [Interval a]
getSubIntervals Leaf (s', e')
  | s' > e' = [] -- Empty range
  | otherwise = [Interval s' e' 0] -- Not in tree -> 0 delta
getSubIntervals (Node (Interval s e d) left right) (s', e')
  | s' > e' = [] -- Empty range
  | s' > e = getSubIntervals right (s', e') -- No overlap
  | e' < s = getSubIntervals left (s', e') -- No overlap
  | s' >= s && e' <= e = [Interval s' e' d] -- fully contained
  | s' < s && e' > e = Interval s e d : getSubIntervals left (s', s - 1) ++ getSubIntervals right (e + 1, e') -- fully encompassed
  | s' < s && e' <= e = Interval s e' d : getSubIntervals left (s', s - 1) -- left overlap
  | s' >= s && e' > e = Interval s' e d : getSubIntervals right (e + 1, e') -- right overlap
  | otherwise = error $ trace (show (s, e, s', e')) "Invalid interval"

seedsToRanges :: (Num a) => [a] -> [Range a]
seedsToRanges [] = []
seedsToRanges (x : y : xs) = (x, x + y - 1) : seedsToRanges xs
seedsToRanges _ = error "Invalid seeds"

--- Parsing

parseData :: Text -> Problem
parseData input =
  case parse parseAll "" input of
    Left err -> error (show err)
    Right (trees, seeds) -> (trees, seeds)

parseTree :: Parser (IntervalTree Int)
parseTree = do
  _ <- many1 letter *> string "-to-" *> many1 letter *> string " map:" *> newline
  mappings <- (many1 digit `sepBy` char ' ') `sepBy1` newline

  return . buildTree . mapMaybe (toInterval . fmap read) $ mappings

toInterval :: (Num a) => [a] -> Maybe (Interval a)
toInterval [dest, src, sz] = Just $ Interval src (src + sz - 1) (dest - src)
toInterval _ = Nothing

parseSeeds :: Parser [Int]
parseSeeds = string "seeds: " *> (read <$> many1 digit) `sepBy` char ' ' <* newline

parseAll :: Parser Problem
parseAll = do
  seeds <- parseSeeds <* spaces
  mappings <- parseTree `sepBy1` spaces <* eof

  return (mappings, seeds)

--- IO

run :: FilePath -> (Problem -> a) -> IO a
run path f = do
  input <- decodeUtf8 <$> readFileBS path
  let p = parseData input

  return $ f p

run1, run2 :: FilePath -> IO Int
run1 = flip run part1
run2 = flip run part2
