{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Day20 where

import Control.Monad.RWS hiding (state)
import Data.Char (isAlpha)
import qualified Data.Map as M
import Relude hiding (state)
import Text.ParserCombinators.ReadP hiding (get)

data ModuleState = BroadCaster Int | FlipFlop Bool | Conjunction (Map String Bool) | Sink deriving (Show)

data Module = Module {state :: ModuleState, outputs :: [String]} deriving (Show)

type ModuleMap = M.Map String Module

newtype Lcm a = Lcm {getLcm :: a} deriving (Eq, Ord, Read, Show, Bounded, Generic, Num)

instance (Integral a) => Semigroup (Lcm a) where
  Lcm a <> Lcm b = Lcm (lcm a b)

instance (Integral a) => Monoid (Lcm a) where
  mempty = Lcm 1

  mconcat = foldl' (<>) mempty
  {-# INLINE mconcat #-}

parseModule :: ReadP (String, Module)
parseModule = do
  name <- munch1 (/= ' ') <* string " -> "
  outputs' <- munch1 isAlpha `sepBy` string ", "

  let (name', state') = case name of
        "broadcaster" -> ("broadcaster", BroadCaster 0)
        ('%' : name'') -> (name'', FlipFlop False)
        ('&' : name'') -> (name'', Conjunction M.empty)
        _ -> error "Invalid module name"

  pure (name', Module state' outputs')

parseModules :: ReadP ModuleMap
parseModules = M.fromList <$> parseModule `sepBy` char '\n' <* char '\n' <* eof

parse :: ByteString -> Maybe ModuleMap
parse = fmap (fixConjunctions . fst) . viaNonEmpty last . readP_to_S parseModules . decodeUtf8

fixConjunctions :: ModuleMap -> ModuleMap
fixConjunctions modules = M.mapWithKey addInputs modules
  where
    addInputs name module' = case state module' of
      BroadCaster _ -> module'
      FlipFlop _ -> module'
      Conjunction _ -> module' {state = Conjunction $ M.fromList $ (,False) <$> findInputs modules name}
      Sink -> module'

findInputs :: ModuleMap -> String -> [String]
findInputs modules name = M.keys $ M.filter (elem name . outputs) modules

pulse :: ModuleState -> Bool -> String -> (ModuleState, Maybe Bool)
pulse (BroadCaster n) b _ = (BroadCaster (n + 1), Just b)
pulse (FlipFlop s) True _ = (FlipFlop s, Nothing)
pulse (FlipFlop s) False _ = let s' = not s in (FlipFlop s', Just s')
pulse (Conjunction m) b from
  | M.foldl (&&) True m' = (Conjunction m', Just False)
  | otherwise = (Conjunction m', Just True)
  where
    m' = M.insert from b m
pulse Sink _ _ = (Sink, Nothing)

-- src, dst, signal
type Signal = (String, String, Bool)

pulse' :: Module -> Signal -> (Module, [Signal])
pulse' m@(Module s o) (from, name, b) = (m', pulses)
  where
    (s', signal) = pulse s b from
    m' = m {state = s'}
    pulses = case signal of
      Nothing -> []
      Just b' -> do
        -- when (name == "sf") $ do
        -- let (Conjunction l) = s'
        -- when (foldl' (&&) True $ M.elems l) $ traceShowM $ M.lookup "broadcaster"
        (name,,b') <$> o

type ModuleM = RWS [String] (Sum Int, Sum Int, Lcm Int) (ModuleMap, [Signal])

pulseM :: ModuleM Bool
pulseM = fmap isJust . runMaybeT $ do
  signal@(_, name, v) <- popQueue
  modules <- gets fst
  (module'', signals) <- lift $ (`pulse'` signal) <$> getModule name
  watching <- asks $ elem name
  when (watching && not v) $ case M.lookup "broadcaster" modules of
    Just (Module (BroadCaster n) _) -> tell (mempty, mempty, Lcm n)
    _ -> pure ()
  lift $ updateModule name module'' *> pushQueue signals

popQueue :: MaybeT ModuleM Signal
popQueue = do
  (s : ss) <- gets snd
  modify (second $ const ss) $> s

pushQueue :: [Signal] -> ModuleM ()
pushQueue s = modify (second (++ s)) *> countPulses s
  where
    countPulses :: [Signal] -> ModuleM ()
    countPulses [] = pure ()
    countPulses ((_, _, b) : ss)
      | b = tell (mempty, Sum n, mempty)
      | otherwise = tell (Sum n, mempty, mempty)
      where
        n = 1 + length ss

updateModule :: String -> Module -> ModuleM ()
updateModule name module' = modify (first $ M.insert name module')

getModule :: String -> ModuleM Module
getModule name = gets fst >>= maybe (pure $ Module Sink []) pure . M.lookup name

pulseTillStable :: ModuleM ()
pulseTillStable = pulseM >>= (`when` pulseTillStable)

runBroadcast :: ModuleM ()
runBroadcast = runButton "broadcaster" $> ()

runButton :: String -> ModuleM ()
runButton target = do
  pushQueue [("button", target, False)] *> pulseTillStable

part1 :: ModuleMap -> Int
part1 modules = go 1000
  where
    go n = getSum . (\(a, b, _) -> a * b) . snd $ evalRWS (replicateM_ n runBroadcast) [] (modules, [])

-- Part 2

-- rx recieves a signal from 4 different binary counters, we check when each one
-- of them reaches the value to trigger the signal and compute the lcm of those
--
-- See graphviz file for a visual representation of the circuit
getParents :: ModuleMap -> [String]
getParents modules = up "rx" >>= up
  where
    up = findInputs modules

part2 :: ModuleMap -> Int
part2 modules = (\(_, _, l) -> getLcm l) $ go 4096
  where
    parents = getParents modules
    go n = snd $ evalRWS (replicateM_ n runBroadcast) parents (modules, [])
