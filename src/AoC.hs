{-# LANGUAGE TemplateHaskell #-}

module AoC
  ( module Day1,
    runSolution,
    Problem (..),
    year,
    day,
    part,
  )
where

import Control.Lens
import Data.Text
import Day1
import Network.Wreq
import Relude hiding (get)

data Problem = Problem
  { _year :: Int,
    _day :: Int,
    _part :: Int
  }

makeLenses ''Problem

runSolution :: Problem -> (Text -> a) -> IO a
runSolution p f = do
  let url = "https://adventofcode.com/" ++ show (p ^. year) ++ "/day/" ++ show (p ^. day) ++ "/input"
  r <- get url
  return . f . decodeUtf8 $ r ^. responseBody
