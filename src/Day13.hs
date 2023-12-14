module Day13 where

import Data.Text (dropEnd, splitOn)
import Relude

type Pattern = [[Bool]]

type Problem = [Pattern]

parseP :: Text -> Problem
parseP = fmap (fmap (fmap (== '#') . toString) . splitOn "\n") . splitOn "\n\n" . dropEnd 1

showPattern :: Pattern -> IO ()
showPattern = mapM_ (putStrLn . fmap (\x -> if x then '#' else '.'))

toInt :: [Bool] -> Int
toInt = foldl' (\acc x -> acc * 2 + if x then 1 else 0) 0

findReflection :: (Eq a) => [a] -> Maybe ([a], [a])
findReflection xs = find isMirrorSplit (splits xs)

-- Generate all possible non-empty splits o a list
splits :: [a] -> [([a], [a])]
splits xs = fromMaybe [] . viaNonEmpty init . drop 1 $ zip (inits xs) (tails xs)

-- Zip the splits together with the first one reversed and check if they are equal
isMirrorSplit :: (Eq a) => ([a], [a]) -> Bool
isMirrorSplit (a, b) = and $ zipWith (==) (reverse a) b

bitDiff :: [Bool] -> [Bool] -> Int
bitDiff a b = length (filter id $ zipWith (/=) a b)

part1 :: Problem -> Int
part1 = sum . fmap (fromEither . first (* 100)) . mapMaybe getReflection

fromEither :: Either a a -> a
fromEither = either id id

getReflection :: Pattern -> Maybe (Either Int Int)
getReflection p = do
  -- let pV = toInt <$> p
  -- let pH = toInt <$> transpose p
  let pV = p
  let pH = transpose p

  case (findReflection pV, findReflection pH) of
    (Just (a, _), _) -> pure . Left $ length a
    (_, Just (a, _)) -> pure . Right $ length a
    _ -> Nothing

part2'' = fmap fromEither . mapMaybe getReflection'

part2' = mapMaybe getReflection'

isMirrorSplit' :: ([[Bool]], [[Bool]]) -> Bool
isMirrorSplit' (a, b) = (== 1) . sum $ zipWith bitDiff (reverse a) b

findReflection' :: [[Bool]] -> Maybe ([[Bool]], [[Bool]])
findReflection' xs = find isMirrorSplit' (splits xs)

getReflection' :: Pattern -> Maybe (Either Int Int)
getReflection' p = do
  -- let pV = toInt <$> p
  -- let pH = toInt <$> transpose p
  let pV = p
  let pH = transpose p

  case (findReflection' pV, findReflection' pH) of
    (Just (a, _), Just (b, _)) -> pure $ traceShow (a, b) $ if length a < length b then Left $ length a else Right $ length b
    (Just (a, _), _) -> pure . Left $ length a
    (_, Just (a, _)) -> pure . Right $ length a
    _ -> Nothing

part2 :: Problem -> Int
part2 = sum . fmap (fromEither . first (* 100)) . mapMaybe getReflection'

-- getReflection' :: Pattern -> Maybe (Either Int Int)
-- getReflection' p = do
--   oldRef <- getReflection p
--   find (/= oldRef) . mapMaybe getReflection $ allFlips p
