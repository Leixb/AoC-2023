module Common (Part (..), Solution (..)) where

import Relude

data Part = Part1 | Part2

class Solution a where
  runPart :: a -> Part -> Text -> String
