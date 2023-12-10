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

-- part1 :: Problem -> Int
part1 p = let (_, _, w) = runRWS runTillEnd p (fromMaybe (0, 0) $ getStart p, getStartDir p) in length w `div` 2

-- run1 :: FilePath -> IO Int
run1 f = part1 <$> (parse . decodeUtf8 <$> readFileBS f)
