{-# LANGUAGE LambdaCase #-}

module Day19 where

import Data.Array (Ix (inRange))
import Data.Char (isAlpha, isDigit, isLowerCase)
import qualified Data.Map as M
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP

type Problem = (Dict, [Xmas])

type Dict = M.Map String Workflow

type Range = (Int, Int)

data Field = X | M | A | S deriving (Show, Eq, Ord)

data XmasT a = Xmas {x, m, a, s :: a} deriving (Show, Eq, Ord)

type Xmas = XmasT Int

type XmasR = XmasT Range

data Veredict = Accept | Reject | Forward String deriving (Show, Eq)

data Rule = Rule
  { variable :: Field,
    operation :: Ordering,
    threshold :: Int,
    veredict :: Veredict
  }
  deriving (Show, Eq)

data Workflow = Workflow
  { rules :: [Rule],
    def :: Veredict
  }
  deriving (Show, Eq)

size :: Range -> Int
size (l, r) = l - r + 1

(!) :: XmasT a -> Field -> a
v ! X = x v
v ! M = m v
v ! A = a v
v ! S = s v

parseRule :: ReadP Rule
parseRule = do
  t <- choice [char 'x' $> X, char 'm' $> M, char 'a' $> A, char 's' $> S]
  o <- (char '>' $> GT) +++ (char '<' $> LT)
  n <- Unsafe.read <$> munch1 isDigit
  out <- char ':' *> parseVeredict

  pure $ Rule t o n out

parseVeredict :: ReadP Veredict
parseVeredict =
  munch1 isAlpha >>= \case
    "A" -> pure Accept
    "R" -> pure Reject
    label -> pure $ Forward label

parseWorkflow :: ReadP (String, Workflow)
parseWorkflow = do
  label <- munch1 isLowerCase <* char '{'
  rules <- many1 (parseRule <* char ',')
  def <- parseVeredict <* char '}'
  pure (label, Workflow rules def)

-- This assumes that xmas is in order
parseXmas :: ReadP Xmas
parseXmas = between (char '{') (char '}') $ do
  x' <- string "x=" *> (Unsafe.read <$> munch1 isDigit <* char ',')
  m' <- string "m=" *> (Unsafe.read <$> munch1 isDigit <* char ',')
  a' <- string "a=" *> (Unsafe.read <$> munch1 isDigit <* char ',')
  s' <- string "s=" *> (Unsafe.read <$> munch1 isDigit)
  pure $ Xmas x' m' a' s'

parseProblem :: ReadP Problem
parseProblem = do
  workflows <- sepBy1 parseWorkflow (char '\n') <* char '\n' <* char '\n'
  xmas <- sepBy1 parseXmas (char '\n') <* char '\n' <* eof
  pure (M.fromList workflows, xmas)

parse :: ByteString -> Maybe Problem
parse = fmap fst . viaNonEmpty head . readP_to_S parseProblem . decodeUtf8

applyRule :: Xmas -> Rule -> Maybe Veredict
applyRule xmas (Rule var op thr ver) = guard ((xmas ! var) `compare` thr == op) *> Just ver

applyWorkflow :: Workflow -> Xmas -> Veredict
applyWorkflow (Workflow rules def) xmas = fromMaybe def . viaNonEmpty head $ mapMaybe (applyRule xmas) rules

getVeredict :: Dict -> String -> Xmas -> Veredict
getVeredict dict label xmas = case applyWorkflow w xmas of
  Forward label' -> getVeredict dict label' xmas
  res -> res
  where
    w = dict M.! label

toBool :: Veredict -> Bool
toBool Accept = True
toBool Reject = False
toBool e = error $ "Unexpected veredict: " <> show e

value :: Xmas -> Int
value (Xmas x' m' a' s') = x' + m' + a' + s'

part1 :: Problem -> Int
part1 (dict, xmas) = sum $ value <$> filter (toBool . getVeredict dict "in") xmas
