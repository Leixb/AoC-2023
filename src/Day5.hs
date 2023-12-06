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

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
  | x < y = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

data Interval a = Interval {start :: a, end :: a, delta :: a} deriving (Show, Eq, Ord, Functor)

type IntervalTree a = Tree (Interval a)

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

toInterval :: (Num a) => [a] -> Maybe (Interval a)
toInterval [dest, src, sz] = Just $ Interval src (src + sz - 1) (dest - src)
toInterval _ = Nothing

parseMapping :: Parser (IntervalTree Int)
parseMapping = do
  _ <- many1 letter *> string "-to-" *> many1 letter *> string " map:" *> newline
  mappings <- (many1 digit `sepBy` char ' ') `sepBy1` newline

  return . buildTree . mapMaybe (toInterval . fmap read) $ mappings

parseSeeds :: Parser [Int]
parseSeeds = string "seeds: " *> (read <$> many1 digit) `sepBy` char ' ' <* newline

parseAll :: Parser ([IntervalTree Int], [Int])
parseAll = do
  seeds <- parseSeeds <* spaces
  mappings <- parseMapping `sepBy1` spaces <* eof

  return (mappings, seeds)

toSumTree :: IntervalTree a -> IntervalTree (Sum a)
toSumTree = fmap (fmap Sum)

withSum :: (Sum a -> Sum a) -> a -> a
withSum f = getSum . f . Sum

run2 :: FilePath -> IO (Int -> Int)
run2 path = do
  input <- decodeUtf8 <$> readFileBS path
  case parse parseAll "" input of
    Left err -> error (show err)
    Right (trees, seeds) -> return $ withSum $ composeTrees $ toSumTree <$> trees

type Problem = ([IntervalTree Int], [Int])

run :: FilePath -> (Problem -> Int) -> IO Int
run path f = do
  input <- decodeUtf8 <$> readFileBS path
  let p = parseData input

  return $ f p

part1 :: Problem -> Int
part1 (trees, seeds) =
  let f = withSum (composeTrees $ toSumTree <$> trees)
   in minimum $ f <$> seeds

run1 :: FilePath -> IO Int
run1 = flip run part1

parseData :: Text -> Problem
parseData input =
  case parse parseAll "" input of
    Left err -> error (show err)
    Right (trees, seeds) -> (trees, seeds)
