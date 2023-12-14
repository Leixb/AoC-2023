module Day14 where

import qualified Data.ByteString.Char8 as BS
import Relude

type Problem = [ByteString]

parse :: ByteString -> Problem
parse = BS.split '\n'

-- Our "north" is to the right, since it makes the problem easier
faceNorth :: Problem -> Problem
faceNorth = rotate

move :: Problem -> Problem
move = fmap (BS.intercalate "#" . fmap BS.sort . BS.split '#')

count :: Problem -> [[Int]]
count = fmap (fmap succ . BS.elemIndices 'O')

part1 :: ByteString -> Int
part1 = sum . join . count . move . faceNorth . parse

rotate :: Problem -> Problem
rotate = fmap BS.reverse . BS.transpose

-- Anti-clockwise
rotate' :: Problem -> Problem
rotate' = BS.transpose . fmap BS.reverse

display :: Problem -> IO ()
display = traverse_ (putStrLn . BS.unpack)

rotMov :: Problem -> Problem
rotMov = rotate . move

doCycle :: Problem -> Problem
doCycle = rotMov . rotMov . rotMov . rotMov

doNcycles :: Int -> Problem -> Problem
doNcycles n = foldr (.) id (replicate n doCycle)

part2 = fmap count . iterate doCycle . faceNorth . parse
