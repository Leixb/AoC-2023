module Day14 where

import qualified Data.ByteString.Char8 as BS
import Relude

part1 :: ByteString -> Int
part1 = sum . fmap (sum . fmap succ . BS.elemIndices 'O' . BS.reverse . BS.intercalate "#" . fmap (BS.reverse . BS.sort) . BS.split '#') . BS.transpose . BS.split '\n'
