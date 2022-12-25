{-# LANGUAGE BlockArguments #-}

module Day4 where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Interval = (Int, Int)

solve :: (Interval -> Interval -> Bool) -> IO Int
solve check =
  T.readFile "src/Day4.txt"
    <&> ( T.lines
            >>> map
              ( T.split (== ',')
                  >>> map (T.split (== '-') >>> map (T.unpack >>> read))
                  >>> \[[a, b], [c, d]] -> fromEnum $ check (a, b) (c, d)
              )
            >>> sum
        )

part1 = solve \(a, b) (c, d) -> (a, b) `contains` (c, d) || (c, d) `contains` (a, b)

part2 = solve overlaps

contains :: Interval -> Interval -> Bool
(a, b) `contains` (c, d) = a <= c && d <= b

overlaps :: Interval -> Interval -> Bool
overlaps (a, b) (c, d) = max a c <= min b d

--- >>> part1
-- 602

--- >>> part2
-- 891
