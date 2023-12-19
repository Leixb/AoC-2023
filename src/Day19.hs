{-# LANGUAGE LambdaCase #-}

module Day19 where

import Data.Char (isAlpha, isDigit, isLowerCase)
import qualified Data.Map as M
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP

data Xmas = Xmas {x, m, a, s :: Int} deriving (Show, Eq, Ord)

data Veredict = Accept | Reject | Forward String deriving (Show, Eq)

type Rule = Xmas -> Maybe Veredict

type Problem = (Dict, [Xmas])

type Dict = M.Map String Rule

toWorkflow :: [Rule] -> Rule
toWorkflow r xmas = asum $ ($ xmas) <$> r

parseRule :: ReadP Rule
parseRule = do
  t <- choice [char 'x' $> x, char 'm' $> m, char 'a' $> a, char 's' $> s]
  f <- (char '>' $> (>)) +++ (char '<' $> (<))
  n <- Unsafe.read <$> munch1 isDigit
  out <- char ':' *> parseVeredict

  pure $ \xmas -> if f (t xmas) n then Just out else Nothing

parseVeredict :: ReadP Veredict
parseVeredict =
  munch1 isAlpha >>= \case
    "A" -> pure Accept
    "R" -> pure Reject
    label -> pure $ Forward label

parseRuleOrVeredict :: ReadP Rule
parseRuleOrVeredict = parseRule <++ (const . Just <$> parseVeredict)

parseWorkflow :: ReadP (String, Rule)
parseWorkflow = do
  label <- munch1 isLowerCase <* char '{'
  rules <- sepBy1 parseRuleOrVeredict (char ',') <* char '}'
  pure (label, toWorkflow rules)

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

getVeredict :: Dict -> String -> Xmas -> Maybe Veredict
getVeredict dict label xmas = case dict M.! label $ xmas of
  Just (Forward label') -> getVeredict dict label' xmas
  res -> res

toBool :: Maybe Veredict -> Bool
toBool (Just Accept) = True
toBool (Just Reject) = False
toBool e = error $ "Unexpected veredict: " <> show e

value :: Xmas -> Int
value (Xmas x' m' a' s') = x' + m' + a' + s'

part1 :: Problem -> Int
part1 (dict, xmas) = sum $ value <$> filter (toBool . getVeredict dict "in") xmas
