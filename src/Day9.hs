{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day9 where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import qualified Data.Set as S

solve i =
  readFile "src/Day9.txt"
    <&> ( lines
            >>> concatMap
              ( \(words -> [d, read -> n]) ->
                  replicate n $ case d of
                    "U" -> (0, 1)
                    "R" -> (1, 0)
                    "D" -> (0, -1)
                    "L" -> (-1, 0)
              )
            >>> scanl (\(!x, !y) (!dx, !dy) -> (x + dx, y + dy)) origin
            >>> iterate
              ( scanl
                  ( \(!tx, !ty) (hx, hy) ->
                      let (dx, dy) = (hx - tx, hy - ty)
                       in if abs dx > 1 || abs dy > 1
                            then (tx + signum dx, ty + signum dy)
                            else (tx, ty)
                  )
                  origin
              )
            >>> (!! i)
            >>> S.fromList
            >>> S.size
        )
  where
    origin :: (Int, Int)
    origin = (0, 0)

part1 = solve 1
part2 = solve 9

-- >>> part1
-- 5735

-- >>> part2
-- 2478