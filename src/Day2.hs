{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

val :: T.Text -> Int
val = \case
  "A" -> 1
  "X" -> 1
  "B" -> 2
  "Y" -> 2
  "C" -> 3
  "Z" -> 3

solve play =
  T.readFile "src/Day2.txt"
    <&> ( T.lines
            >>> map (T.words >>> map val >>> play)
            >>> sum
        )

part1 :: IO Int
part1 = solve (\[a, b] -> b + [6, 0, 3, 6, 0] !! (b + 2 - a))

part2 :: IO Int
part2 = solve (\[a, b] -> (b - 1) * 3 + [3, 1, 2, 3, 1] !! (a + b - 2))

-- >>> part1
-- 12458

-- >>> part2
-- 12683
