module Day14 where

import qualified Data.ByteString.Char8 as BS
import Relude

type Problem = [ByteString]

parse :: ByteString -> Problem
parse = BS.split '\n'

faceNorth :: Problem -> Problem
faceNorth = rotate . rotate . BS.transpose

move :: Problem -> Problem
move = fmap (BS.intercalate "#" . fmap BS.sort . BS.split '#')

count :: Problem -> [[Int]]
count = fmap (fmap succ . BS.elemIndices 'O')

part1 :: ByteString -> Int
part1 = sum . join . count . move . faceNorth . parse

rotate :: Problem -> Problem
rotate = BS.transpose . fmap BS.reverse
