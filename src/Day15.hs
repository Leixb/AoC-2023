module Day15 where

import qualified Data.ByteString.Char8 as BS
import Relude

part1 :: BS.ByteString -> Int
part1 = sum . fmap ((foldl' step 0 . fmap ord) . BS.unpack) . BS.split ',' . BS.dropEnd 1
  where
    step prev n = (prev + n) * 17 `mod` 256
