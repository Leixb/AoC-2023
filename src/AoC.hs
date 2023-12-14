{-# LANGUAGE TemplateHaskell #-}

module AoC
  ( getInput,
    getInputBS,
    getInputString,
    Problem (..),
    year,
    day,
  )
where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Network.Wreq (defaults, getWith, header, responseBody)
import Relude hiding (get)

data Problem = Problem
  { _year :: Int,
    _day :: Int
  }

makeLenses ''Problem

getCookie :: IO ByteString
getCookie = readFileBS "cookie.txt" <&> ("session=" <>) . BS.init

getInputBS :: Problem -> IO ByteString
getInputBS p = do
  let url = "https://adventofcode.com/" ++ show (p ^. year) ++ "/day/" ++ show (p ^. day) ++ "/input"
  cookie <- getCookie
  r <- getWith (defaults & header "Cookie" .~ [cookie]) url
  return . toStrict $ r ^. responseBody

getInput :: Problem -> IO Text
getInput = fmap decodeUtf8 . getInputBS

getInputString :: Problem -> IO String
getInputString = fmap T.unpack . getInput
