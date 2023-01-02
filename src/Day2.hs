{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import qualified Data.ByteString.Char8 as B

solve play =
  B.readFile "src/Day2.txt"
    <&> ( B.lines
            >>> map (B.words >>> map val >>> play)
            >>> sum
        )

-- >>> part1
-- 12458

-- >>> part2
-- 12683

part1 :: IO Int
part1 = solve (\[a, b] -> b + [6, 0, 3, 6, 0] !! (b + 2 - a))

part2 :: IO Int
part2 = solve (\[a, b] -> (b - 1) * 3 + [3, 1, 2, 3, 1] !! (a + b - 2))

val :: B.ByteString -> Int
val = \case
  "A" -> 1
  "X" -> 1
  "B" -> 2
  "Y" -> 2
  "C" -> 3
  "Z" -> 3
