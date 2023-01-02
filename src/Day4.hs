{-# LANGUAGE BlockArguments #-}

module Day4 where

import Control.Arrow ((>>>))
import qualified Data.ByteString.Char8 as B
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Interval = (Int, Int)

solve :: (Interval -> Interval -> Bool) -> IO Int
solve check =
  B.readFile "src/Day4.txt"
    <&> ( B.lines
            >>> map
              ( B.split ','
                  >>> map (B.split '-' >>> map B.readInt)
                  >>> \[[Just (a, _), Just (b, _)], [Just (c, _), Just (d, _)]] -> fromEnum $ check (a, b) (c, d)
              )
            >>> sum
        )

--- >>> part1
-- 602

--- >>> part2
-- 891

part1 = solve \(a, b) (c, d) -> (a, b) `contains` (c, d) || (c, d) `contains` (a, b)

part2 = solve overlaps

contains :: Interval -> Interval -> Bool
(a, b) `contains` (c, d) = a <= c && d <= b

overlaps :: Interval -> Interval -> Bool
overlaps (a, b) (c, d) = max a c <= min b d
