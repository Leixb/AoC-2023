{-# LANGUAGE LambdaCase #-}

module Day16 where

import Control.Monad.RWS
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Set
import Relude
import qualified Relude.Unsafe as Unsafe

data Cell = Empty | VertSplitter | HorizSplitter | Slash | Backslash deriving (Show, Eq)

type Pos = (Int, Int)

type Grid = Array Pos Cell

data Direction = N | S | E | W deriving (Show, Eq, Ord)

data BeamHead = BeamHead
  { pos :: Pos,
    dir :: Direction
  }
  deriving (Show, Eq, Ord)

data Beams = Beams
  { seen :: Set BeamHead,
    heads :: [BeamHead]
  }
  deriving (Show, Eq)

type Simulation = RWS Grid (Set Pos) Beams

initialBeams :: Beams
initialBeams = Beams mempty [BeamHead (0, 0) E]

runSimulation :: Grid -> Set Pos
runSimulation l = snd $ evalRWS run l initialBeams

-- part1 :: Grid -> Int
-- part1 = size . runSimulation

part1 = runSimulation

next :: BeamHead -> BeamHead
next (BeamHead p d) = BeamHead (next' p d) d
  where
    next' :: Pos -> Direction -> Pos
    next' (y, x) direction = case direction of
      N -> (y - 1, x)
      S -> (y + 1, x)
      E -> (y, x + 1)
      W -> (y, x - 1)

advance :: BeamHead -> Simulation [BeamHead]
advance bh@(BeamHead pos dir) = do
  grid <- ask
  seen' <- gets seen

  if inRange (bounds grid) pos && bh `notMember` seen'
    then do
      tell $ singleton pos
      modify $ \beams -> beams {seen = insert bh seen'}
      case (grid ! pos, dir) of
        (Empty, _) -> pure [next bh]
        (VertSplitter, N) -> pure [next bh]
        (VertSplitter, S) -> pure [next bh]
        (HorizSplitter, E) -> pure [next bh]
        (HorizSplitter, W) -> pure [next bh]
        (VertSplitter, _) -> pure [next $ bh {dir = N}, next $ bh {dir = S}]
        (HorizSplitter, _) -> pure [next $ bh {dir = E}, next $ bh {dir = W}]
        (Slash, N) -> pure [next $ bh {dir = E}]
        (Slash, S) -> pure [next $ bh {dir = W}]
        (Slash, E) -> pure [next $ bh {dir = N}]
        (Slash, W) -> pure [next $ bh {dir = S}]
        (Backslash, N) -> pure [next $ bh {dir = W}]
        (Backslash, S) -> pure [next $ bh {dir = E}]
        (Backslash, E) -> pure [next $ bh {dir = S}]
        (Backslash, W) -> pure [next $ bh {dir = N}]
    else pure []

advanceAll :: Simulation ()
advanceAll = do
  beams <- gets heads
  beams' <- concat <$> traverse advance beams
  modify $ \b -> b {heads = beams'}

run :: Simulation ()
run = do
  loop
  where
    loop = do
      advanceAll
      beams <- gets heads
      unless (Relude.null beams) loop

toCell :: Char -> Maybe Cell
toCell '.' = Just Empty
toCell '|' = Just VertSplitter
toCell '-' = Just HorizSplitter
toCell '/' = Just Slash
toCell '\\' = Just Backslash
toCell _ = Nothing

parse :: ByteString -> Maybe Grid
parse input = do
  let ls = BS.lines input
      n = length ls
  m <- BS.length <$> viaNonEmpty head ls
  mat <- traverse toCell . BS.unpack $ BS.concat ls
  pure $ listArray ((0, 0), (n - 1, m - 1)) mat

display :: Grid -> IO ()
display mat =
  let ((x0, y0), (x1, y1)) = bounds mat
   in forM_ [x0 .. x1] $ \i -> do
        forM_ [y0 .. y1] $ \j -> do
          putStr $ case mat ! (i, j) of
            Empty -> "."
            VertSplitter -> "|"
            HorizSplitter -> "-"
            Slash -> "/"
            Backslash -> "\\"
        putStrLn ""

displaySeen :: Grid -> Set Pos -> IO ()
displaySeen g seen = do
  let ((x0, y0), (x1, y1)) = bounds g
  forM_ [x0 .. x1] $ \i -> do
    forM_ [y0 .. y1] $ \j -> do
      putStr $ if (i, j) `member` seen then "X" else "."
    putStrLn ""

part1' :: FilePath -> IO ()
part1' file = do
  input <- readFileBS file
  let grid = Unsafe.fromJust $ parse input
  let seen = runSimulation grid
  displaySeen grid seen
  print $ size seen
