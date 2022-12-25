{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Day10 where

import Control.Arrow ((>>>))
import qualified Data.Array as A
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (groupBy, mapAccumL)
import Data.Maybe (mapMaybe)

trace =
  readFile "src/Day10.txt"
    <&> ( lines
            >>> mapAccumL
              ( \(!i, !x) ln -> case words ln of
                  ["addx", read -> d] -> ((i + 2, x + d), [(i + 1, x), (i + 2, x)])
                  ["noop"] -> ((i + 1, x), [(i + 1, x)])
              )
              (0 :: Int, 1 :: Int)
            >>> snd
            >>> concat
        )

part1 =
  trace
    <&> ( mapMaybe
            ( \(i, x) ->
                if (i - 20) `rem` 40 == 0
                  then Just $ i * x
                  else Nothing
            )
            >>> sum
        )

part2 =
  trace
    <&> ( mapMaybe
            ( \(i, x) ->
                let (r, c) = (i - 1) `quotRem` 40
                 in if c `elem` [x - 1 .. x + 1] then Just ((r, c), '#') else Nothing
            )
            >>> A.accumArray (\_ x -> x) '.' ((0, 0), (5, 39))
            >>> A.assocs
            >>> groupBy ((==) `on` fst . fst)
            >>> map (map snd)
            >>> unlines
        )

-- >>> part1
-- 12640

-- >>> part2 >>= putStrLn
