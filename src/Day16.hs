module Day16 (part1, part2, parse) where

import Control.Monad.RWS
import Control.Parallel.Strategies
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (Foldable (maximum))
import Data.Set
import Relude

data Cell = Empty | VertSplitter | HorizSplitter | Slash | Backslash deriving (Show, Eq)

type Pos = (Int, Int)

type Grid = Array Pos Cell

data Direction = N | S | E | W deriving (Show, Eq, Ord)

data BeamHead = BeamHead
  { pos :: Pos,
    dir :: Direction
  }
  deriving (Show, Eq, Ord)

type Simulation = RWS Grid (Set Pos) (Set BeamHead)

next :: BeamHead -> BeamHead
next (BeamHead p d) = BeamHead (next' d p) d
  where
    next' :: Direction -> Pos -> Pos
    next' direction = case direction of
      N -> first pred
      S -> first succ
      E -> second succ
      W -> second pred

advance :: BeamHead -> Simulation [BeamHead]
advance bh@(BeamHead position direction) = do
  grid <- ask
  seen <- get

  if inRange (bounds grid) position && bh `notMember` seen
    then do
      tell $ singleton position
      modify $ insert bh
      pure . fmap next $ case (grid ! position, direction) of
        (Empty, _) -> [bh]
        (VertSplitter, N) -> [bh]
        (VertSplitter, S) -> [bh]
        (HorizSplitter, E) -> [bh]
        (HorizSplitter, W) -> [bh]
        (VertSplitter, _) -> [bh {dir = N}, bh {dir = S}]
        (HorizSplitter, _) -> [bh {dir = E}, bh {dir = W}]
        (Slash, N) -> [bh {dir = E}]
        (Slash, S) -> [bh {dir = W}]
        (Slash, E) -> [bh {dir = N}]
        (Slash, W) -> [bh {dir = S}]
        (Backslash, N) -> [bh {dir = W}]
        (Backslash, S) -> [bh {dir = E}]
        (Backslash, E) -> [bh {dir = S}]
        (Backslash, W) -> [bh {dir = N}]
    else pure []

simulate :: [BeamHead] -> Simulation ()
simulate heads = do
  heads' <- foldMapM advance heads
  unless (Relude.null heads') $ simulate heads'

runSimulation :: BeamHead -> Grid -> Int
runSimulation origin g = size . snd . evalRWS (simulate [origin]) g $ mempty

part1, part2 :: Grid -> Int
part1 = runSimulation $ BeamHead (0, 0) E
part2 g = maximum $ parMap rpar (`runSimulation` g) possibleInitials
  where
    ((y0, x0), (y1, x1)) = bounds g
    possibleInitials =
      join
        [ [BeamHead (y0, x) S | x <- [x0 .. x1]],
          [BeamHead (y1, x) N | x <- [x0 .. x1]],
          [BeamHead (y, x0) E | y <- [y0 .. y1]],
          [BeamHead (y, x1) W | y <- [y0 .. y1]]
        ]

parse :: ByteString -> Maybe Grid
parse input = do
  let ls = BS.lines input
      h = length ls
  w <- BS.length <$> viaNonEmpty head ls
  mat <- traverse toCell . BS.unpack $ BS.concat ls
  pure $ listArray ((0, 0), (h - 1, w - 1)) mat
  where
    toCell '.' = Just Empty
    toCell '|' = Just VertSplitter
    toCell '-' = Just HorizSplitter
    toCell '/' = Just Slash
    toCell '\\' = Just Backslash
    toCell _ = Nothing
