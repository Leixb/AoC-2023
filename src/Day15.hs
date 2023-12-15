{-# LANGUAGE TupleSections #-}

module Day15 where

import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlpha, isDigit)
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP hiding (get)

part1 :: BS.ByteString -> Int
part1 = sum . fmap (hash . BS.unpack) . BS.split ',' . BS.dropEnd 1

hash :: String -> Int
hash = foldl' step 0 . fmap ord
  where
    step prev n = (prev + n) * 17 `mod` 256

-- Part 2

type Problem = [Operation]

data Operation = Set String Int | Remove String deriving (Show)

parse :: BS.ByteString -> Maybe Problem
parse = fmap fst . viaNonEmpty last . readP_to_S parse' . BS.unpack
  where
    parse' = sepBy parseOperation (char ',') <* char '\n' <* eof

    parseOperation = do
      label <- munch1 isAlpha
      (Remove label <$ char '-') +++ (Set label . Unsafe.read <$> (char '=' *> munch1 isDigit))

type App = State (Array Int [(String, Int)])

doOperation :: Operation -> App ()
doOperation op = do
  m <- gets (! idx)
  let (b, xa) = span ((/= label) . fst) m
      a = fromMaybe [] $ viaNonEmpty tail xa
  modify
    $ flip
      (//)
      [ ( idx,
          case op of
            Set _ value -> b <> [(label, value)] <> a
            Remove _ -> b <> a
        )
      ]
  where
    getLabelAndIdx (Set l _) = (l, hash l)
    getLabelAndIdx (Remove l) = (l, hash l)
    (label, idx) = getLabelAndIdx op

score :: App [Int]
score = do
  m <- get
  pure $ join [(* (i + 1)) <$> multByIdx (snd <$> (m ! i)) | i <- [0 .. 255]]
  where
    multByIdx = zipWith (*) [1 ..]

part2 :: BS.ByteString -> Maybe Int
part2 input = do
  ops <- parse input
  pure . sum $ evalState (traverse_ doOperation ops *> score) initialState
  where
    initialState = listArray (0, 256) $ repeat []
