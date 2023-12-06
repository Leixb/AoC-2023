module Day6 where

import Relude

polyroots :: Double -> Double -> (Int, Int)
polyroots b c =
  let a = -1.0
      sd = sqrt $ b * b - 4 * a * (-c)
   in (floor $ (-b - sd) / (2 * a), ceiling $ (-b + sd) / (2 * a))

input = ([48, 93, 85, 95], [296, 1928, 1236, 1391])

part1 = uncurry solve input
  where
    solve xs ys = product $ fmap pred $ zipWith polyroots xs ys <&> uncurry (-)

input2 = (48938595, 296192812361391)

part2 = uncurry (-) . uncurry polyroots $ input2
