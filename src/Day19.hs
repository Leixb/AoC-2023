{-# LANGUAGE LambdaCase #-}

module Day19 (part1, part2, parse) where

import Data.Char (isAlpha, isDigit, isLowerCase)
import Data.List (lookup)
import qualified Data.Map as M
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP

type Problem = (Dict, [Xmas])

data Field = X | M | A | S deriving (Show, Eq, Ord)

data XmasT a = Xmas {x, m, a, s :: a} deriving (Show, Eq, Ord)

type Xmas = XmasT Int

data Veredict = Accept | Reject | Forward String deriving (Show, Eq)

data Rule = Rule
  { variable :: Field,
    operation :: Ordering,
    threshold :: Int,
    veredict :: Veredict
  }
  deriving (Show, Eq)

type Workflow = ([Rule], Veredict)

type Dict = M.Map String Workflow

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
  pure (label, (rules, def))

parseXmas :: ReadP Xmas
parseXmas = between (char '{') (char '}') $ do
  lookupList <- sepBy1 parseLookup (char ',')
  case mapMaybe (`lookup` lookupList) "xmas" of
    [x', m', a', s'] -> pure $ Xmas x' m' a' s'
    _ -> pfail
  where
    parseLookup = do
      c <- satisfy isAlpha <* char '='
      n <- Unsafe.read <$> munch1 isDigit
      pure (c, n)

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
applyWorkflow (r, d) xmas = fromMaybe d . viaNonEmpty head $ mapMaybe (applyRule xmas) r

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

-- Part 2

type Range = (Int, Int)

type XmasR = XmasT Range

-- Returns a pair of ranges, the first one is the range of the values that are
-- not matched by the rule, the second one is the range of the values that are
-- matched by the rule. (the range (a, b) is empty if a > b)
intersect :: Range -> Ordering -> Int -> (Range, Range)
intersect (l, r) op thr
  | op == GT = splitRange (l, r) thr
  | op == LT = swap $ splitRange (l, r) (thr - 1)
  | otherwise = error "Unexpected operation"
  where
    splitRange (l', r') th = ((l', min th r'), (max l' (th + 1), r'))

isEmpty :: XmasR -> Bool
isEmpty xmas = any (uncurry (>) . (xmas !)) [X, M, A, S]

applyRuleR :: XmasR -> Rule -> (XmasR, (XmasR, Veredict))
applyRuleR xmas (Rule v op thr ver) = (update xmas v rest, (update xmas v ok, ver))
  where
    (rest, ok) = intersect (xmas ! v) op thr

applyWorkflowR :: Workflow -> XmasR -> [(XmasR, Veredict)]
applyWorkflowR (rules, def) = go rules def
  where
    go :: [Rule] -> Veredict -> XmasR -> [(XmasR, Veredict)]
    go _ _ xmas | isEmpty xmas = []
    go [] d xmas = [(xmas, d)]
    go (r : rs) d xmas
      | isEmpty (fst ok) = rec
      | otherwise = ok : rec
      where
        (rest, ok) = applyRuleR xmas r
        rec = go rs d rest

part2 :: Problem -> Int
part2 (dict, _) = go (Xmas range range range range, Forward "in")
  where
    range = (1, 4000)
    go :: (XmasR, Veredict) -> Int
    go (xmas, ver) = case ver of
      Forward label -> sum $ go <$> applyWorkflowR (dict M.! label) xmas
      Accept -> size xmas
      Reject -> 0

size :: XmasR -> Int
size xmas | isEmpty xmas = 0
size xmas = product . fmap size' $ (xmas !) <$> [X, M, A, S]
  where
    size' (l, r) | l > r = 0
    size' (l, r) = r - l + 1

update :: XmasT a -> Field -> a -> XmasT a
update xmas X v = xmas {x = v}
update xmas M v = xmas {m = v}
update xmas A v = xmas {a = v}
update xmas S v = xmas {s = v}

(!) :: XmasT a -> Field -> a
v ! X = x v
v ! M = m v
v ! A = a v
v ! S = s v
