module Day1 (part1, part2) where

import Common
import Data.Char
import qualified Data.Text as T
import Relude hiding ((<|>))
import Text.Parsec
import Text.Parsec.Text

data Day1 = Day1

--- Day 1: Trebuchet?! ---

instance Solution Day1 where
  runPart Day1 Part1 = part1
  runPart Day1 Part2 = part2

-- | Common structure to parse problem
doSolve :: (Num b, IsString c, Show b) => (Text -> Maybe b) -> Text -> c
doSolve f = show . sum . mapMaybe f . lines

-- | Specialized calls to solvers
part1, part2 :: Text -> String
part1 = doSolve solve1
part2 = doSolve solve2

solve1, solve2 :: Text -> Maybe Int

-- | Simple solution for part 1 with dropWhile
--
-- Note on the abuse of applicative functors:
-- - liftA2 (+) lifts (+) to work on Maybe Int
-- - the second liftA2 ( in the form : x <$> y <*> z ) is a lifted version of x y z so that
--   the function can be made top free
solve1 = liftA2 (+) <$> fmap (* 10) . firstDigit <*> firstDigit . T.reverse
  where
    firstDigit = fmap (digitToInt . fst) . T.uncons . T.dropWhile isLetter

-- | Parsec solution for part 2
solve2 =
  liftA2 (+)
    <$> fmap (* 10)
    . parseToMaybe (parseNames nums)
    <*> parseToMaybe (parseNames $ reverse <$> nums)
    . T.reverse -- we reverse everything: parser, input and number names
  where
    nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

    parseToMaybe :: Parser a -> Text -> Maybe a
    parseToMaybe p = rightToMaybe . parse p ""

    parseNames :: [String] -> Parser Int
    parseNames names =
      skipUntil
        $ (digitToInt <$> digit)
        <|> asum [try $ string name $> n | (name, n) <- zip names [1 ..]]
      where
        skipUntil :: Parser a -> Parser a
        skipUntil p = try p <|> (anyChar *> skipUntil p)
