module Day5 where

import Control.Applicative
import Data.List (minimum)
import Relude
import Relude.Unsafe (read)
import Text.Parsec
import Text.Parsec.Text

data Mapping = Mapping
  { destStart :: Int,
    sourceStart :: Int,
    size :: Int
  }
  deriving (Show)

applyMapping :: Mapping -> Int -> Maybe Int
applyMapping (Mapping dest src sz) n
  | n >= src && n < src + sz = Just $ n + (dest - src)
  | otherwise = Nothing

defaultMapping :: Mapping
defaultMapping = Mapping 0 0 maxBound

maybeToId :: (Int -> Maybe Int) -> Int -> Int
maybeToId f n = fromMaybe n (f n)

applyMappings :: [Mapping] -> Int -> Maybe Int
applyMappings mappings n = asum (applyMapping <$> mappings <*> pure n)

toMapping :: [Int] -> Maybe Mapping
toMapping [dest, src, sz] = Just (Mapping dest src sz)
toMapping _ = Nothing

parseMapping :: Parser [Mapping]
parseMapping = do
  _ <- many1 letter *> string "-to-" *> many1 letter *> string " map:" *> newline
  mappings <- (many1 digit `sepBy` char ' ') `sepBy1` newline
  let intMappings = fmap (fmap read) mappings :: [[Int]]
      mappings' = mapMaybe toMapping intMappings :: [Mapping]

  return mappings'

parseSeeds :: Parser [Int]
parseSeeds = string "seeds: " *> (read <$> many1 digit) `sepBy` char ' ' <* newline

parseAll :: Parser ([[Mapping]], [Int])
parseAll = do
  seeds <- parseSeeds <* spaces
  mappings <- parseMapping `sepBy1` spaces <* eof

  return (mappings, seeds)

chain :: [[Mapping]] -> Int -> Int
chain = foldl' (flip (.)) id . fmap (maybeToId . applyMappings)

run :: FilePath -> IO Int
run path = do
  input <- decodeUtf8 <$> readFileBS path
  case parse parseAll "" input of
    Left err -> error (show err)
    Right (mappings, seeds) -> return . minimum $ chain mappings <$> seeds
