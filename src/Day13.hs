module Day13 where

import Data.List (elemIndex)
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

part1 :: Problem -> Int
part1 p =
  let pV = fmap (fmap toInt) p
      pH = fmap (fmap toInt . transpose) p
      rV = (* 100) $ sum $ length . fst <$> mapMaybe findReflection pV
      rH = sum $ length . fst <$> mapMaybe findReflection pH
   in rV + rH
