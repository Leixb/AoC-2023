module Day15 where

import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlpha, isDigit)
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP hiding (get)

hash :: String -> Int
hash = foldl' (\a x -> (a + x) * 17 `mod` 256) 0 . fmap ord

part1 :: ByteString -> Int
part1 = sum . fmap (hash . BS.unpack) . BS.split ',' . BS.dropEnd 1

-- Part 2

type Problem = [Operation]

type S = Array Int [(String, Int)]

data Operation = Set String Int | Remove String deriving (Show)

parse :: BS.ByteString -> Maybe Problem
parse = fmap fst . viaNonEmpty last . readP_to_S parse' . BS.unpack
  where
    parse' = sepBy parseOperation (char ',') <* char '\n' <* eof
    parseOperation =
      munch1 isAlpha
        >>= \label -> (Remove label <$ char '-') +++ (Set label . Unsafe.read <$> (char '=' *> munch1 isDigit))

liftOp :: Operation -> Endo S
liftOp (Set label v) = Endo $ \s ->
  let (b, a) = second (drop 1) $ span ((/= label) . fst) (s ! hash label)
   in s // [(hash label, b <> [(label, v)] <> a)]
liftOp (Remove l) = Endo $ \s -> s // [(hash l, filter ((/= l) . fst) (s ! hash l))]

score :: S -> Int
score m = sum $ join [(* (i + 1)) <$> zipWith (*) [1 ..] (snd <$> (m ! i)) | i <- [0 .. 255]]

part2 :: ByteString -> Maybe Int
part2 input = do
  ops <- appEndo . foldMap liftOp . reverse <$> parse input
  pure . score . ops . listArray (0, 255) $ repeat []
