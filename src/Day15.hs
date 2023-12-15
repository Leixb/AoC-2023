module Day15 where

import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlpha, isDigit)
import qualified Data.Map as M
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.ParserCombinators.ReadP hiding (get)

part1 :: BS.ByteString -> Int
part1 = sum . fmap (hash . BS.unpack) . BS.split ',' . BS.dropEnd 1

hash :: String -> Int
hash = foldl' step 0 . fmap ord
  where
    step prev n = (prev + n) * 17 `mod` 256

type Problem = [Operation]

data Operation = Set String Int | Remove String deriving (Show)

parse :: BS.ByteString -> Maybe Problem
parse = fmap fst . viaNonEmpty last . readP_to_S parse' . BS.unpack
  where
    parse' = sepBy parseOperation (char ',') <* char '\n' <* eof

    parseOperation = do
      label <- munch1 isAlpha
      (Remove label <$ char '-') +++ (Set label . Unsafe.read <$> (char '=' *> munch1 isDigit))

type App = State (Array Int (M.Map String Int))

initialState :: Array Int (M.Map String Int)
initialState = listArray (0, 256) $ repeat M.empty

getLabelAndIdx :: Operation -> (String, Int)
getLabelAndIdx (Set label _) = (label, hash label)
getLabelAndIdx (Remove label) = (label, hash label)

doOperation :: Operation -> App ()
doOperation op = do
  let (label, idx) = getLabelAndIdx op
  m <- gets (! idx)
  modify
    $ flip
      (//)
      [ ( idx,
          case op of
            Set _ value -> M.insert label value m
            Remove _ -> M.delete label m
        )
      ]

-- part2 :: BS.ByteString ->
part2 input =
  let ops = Unsafe.fromJust $ parse input
      s = execState (traverse_ doOperation ops) initialState
   in s
