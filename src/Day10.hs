module Day10 where

import Control.Monad.RWS
import Data.List (elemIndex)
import Relude

type Problem = [String]

data Direction = U | D | L | R
  deriving (Show, Eq, Enum)

type Pos = (Int, Int)

type App = RWS Problem [Pos] (Pos, Direction)

parse :: Text -> Problem
parse = fmap toString . lines

getCell :: App (Maybe Char)
getCell = do
  ((x, y), _) <- get
  grid <- ask

  pure $ grid !!? y >>= (!!? x)

nextDir :: Char -> Direction -> Maybe Direction
nextDir '-' L = Just L
nextDir '-' R = Just R
nextDir '|' U = Just U
nextDir '|' D = Just D
nextDir 'L' D = Just R
nextDir 'L' L = Just U
nextDir 'F' U = Just R
nextDir 'F' L = Just D
nextDir '7' U = Just L
nextDir '7' R = Just D
nextDir 'J' D = Just L
nextDir 'J' R = Just U
nextDir _ _ = Nothing

updateDir :: App Bool
updateDir = do
  (pos, dir) <- get
  cell <- getCell
  case cell of
    Just c -> case nextDir c dir of
      Just dir' -> put (pos, dir') $> False
      Nothing -> pure True
    Nothing -> pure True

step :: App Bool
step = do
  (pos, dir) <- get
  tell [pos]
  case dir of
    U -> modify . first $ second (subtract 1)
    D -> modify . first $ second (+ 1)
    L -> modify . first $ first (subtract 1)
    R -> modify . first $ first (+ 1)

  updateDir

runTillEnd :: App ()
runTillEnd = step >>= flip unless runTillEnd

getStart :: Problem -> Maybe Pos
getStart p = do
  x' <- x
  pure (x', y)
  where
    y = length . takeWhile (notElem 'S') $ p
    x = do
      row <- p !!? y
      elemIndex 'S' row

getStartDir :: Problem -> Direction
getStartDir = const R -- TODO: Properly decide start direction

findPath :: Problem -> [Pos]
findPath p = let (_, _, w) = runRWS runTillEnd p (fromMaybe (0, 0) $ getStart p, getStartDir p) in w

part1, part2 :: Problem -> Int
part1 = (`div` 2) . length . findPath

-- PART 2

drawPath :: Problem -> [Pos] -> Problem
drawPath p path = [[if (x, y) `elem` path then c else '.' | (x, c) <- zip [0 ..] row] | (y, row) <- zip [0 ..] p]

isInside :: Pos -> Problem -> Maybe Bool
isInside (x, y) grid = grid !!? y <&> odd . length . filter (`elem` ("|JL" :: String)) . drop x

drawInside :: Problem -> Problem
drawInside p = [[if isInside (x, y) p == Just True && c == '.' then '#' else c | (x, c) <- zip [0 ..] row] | (y, row) <- zip [0 ..] p]

calcInside :: Problem -> Int
calcInside p = sum . join $ [[if isInside (x, y) p == Just True && c == '.' then 1 else 0 | (x, c) <- zip [0 ..] row] | (y, row) <- zip [0 ..] p]

run1, run2 :: FilePath -> IO Int
run1 f = part1 <$> (parse . decodeUtf8 <$> readFileBS f)
run2 f = part2 <$> (parse . decodeUtf8 <$> readFileBS f)

part2 grid = let path = findPath grid in calcInside $ drawPath grid path
