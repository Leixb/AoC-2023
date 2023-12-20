{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day20 where

import Control.Monad.RWS hiding (state)
import Data.Char (isAlpha)
import qualified Data.Map as M
import Relude hiding (state)
import Text.ParserCombinators.ReadP hiding (get)

data ModuleState = BroadCaster | FlipFlop Bool | Conjunction (Map String Bool) | Sink deriving (Show)

data Module = Module {state :: ModuleState, outputs :: [String]} deriving (Show)

type ModuleMap = M.Map String Module

parseModule :: ReadP (String, Module)
parseModule = do
  name <- munch1 (/= ' ') <* string " -> "
  outputs' <- munch1 isAlpha `sepBy` string ", "

  let (name', state') = case name of
        "broadcaster" -> ("broadcaster", BroadCaster)
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
      BroadCaster -> module'
      FlipFlop _ -> module'
      Conjunction _ -> module' {state = Conjunction $ M.fromList $ (,False) <$> findInputs name}
      Sink -> module'

    findInputs name = M.keys $ M.filter (elem name . outputs) modules

pulse :: ModuleState -> Bool -> String -> (ModuleState, Maybe Bool)
pulse BroadCaster b _ = (BroadCaster, Just b)
pulse (FlipFlop s) True _ = (FlipFlop s, Nothing)
pulse (FlipFlop s) False _ = let s' = not s in (FlipFlop s', Just s')
pulse (Conjunction m) b from
  | M.foldr (&&) True m' = (Conjunction m', Just False)
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
      Just b' -> (name,,b') <$> o

type ModuleM = RWS () (Sum Int, Sum Int) (ModuleMap, [Signal])

pulseM :: ModuleM ()
pulseM = fmap void runMaybeT $ do
  signal@(_, name, _) <- popQueue
  (module'', signals) <- lift $ (`pulse'` signal) <$> getModule name
  lift $ updateModule name module'' *> pushQueue signals

popQueue :: MaybeT ModuleM Signal
popQueue = do
  (s : ss) <- gets snd
  modify (second $ const ss) $> s

pushQueue :: [Signal] -> ModuleM ()
pushQueue s = modify (second (++ s)) *> countPulses s

updateModule :: String -> Module -> ModuleM ()
updateModule name module' = modify (first $ M.insert name module')

getModule :: String -> ModuleM Module
getModule name = do
  modules <- gets fst
  pure $ case M.lookup name modules of
    Nothing -> Module Sink []
    Just module' -> module'

countPulses :: [Signal] -> ModuleM ()
countPulses [] = pure ()
countPulses ((_, _, b) : ss) = tell $ if b then (mempty, Sum n) else (Sum n, mempty)
  where
    n = 1 + length ss

pulseTillStable :: ModuleM ()
pulseTillStable = do
  pulseM
  queue <- gets snd
  unless (null queue) pulseTillStable

pushSignal :: Signal -> ModuleM ()
pushSignal s = modify (second (++ [s])) *> countPulses [s]

runBroadcast :: ModuleM ()
runBroadcast = pushSignal ("button", "broadcaster", False) *> pulseTillStable

runBroadcastN :: Int -> ModuleM ()
runBroadcastN = flip replicateM_ runBroadcast

part1 :: ModuleMap -> Int
part1 modules = go 1000
  where
    go n = uncurry (*) . bimap getSum getSum . snd $ evalRWS (runBroadcastN n) () (modules, [])
