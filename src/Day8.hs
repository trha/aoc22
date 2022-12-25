{-# LANGUAGE RankNTypes #-}

module Day8 where

import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Arrow ((>>>))
import Control.Monad.Trans.State.Strict (execState, modify)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List (findIndices, inits, scanl', sortOn, transpose)

part1 =
  readFile "src/Day8.txt"
    <&> ( lines
            >>> \m ->
              let m' = transpose m
               in [ m
                  , m & map (init . scanl' max '/')
                  , m & map (tail . scanr max '/')
                  , m' & map (init . scanl' max '/') & transpose
                  , m' & map (tail . scanr max '/') & transpose
                  ]
                    & map concat
                    & transpose
                    & filter (\(x : xs) -> x > minimum xs)
                    & length
        )

part2 =
  readFile "src/Day8.txt"
    <&> ( lines
            >>> \m ->
              let m' = transpose m
               in [ m & map prevLtDist
                  , m & map (reverse . prevLtDist . reverse)
                  , m' & map prevLtDist & transpose
                  , m' & map (reverse . prevLtDist . reverse) & transpose
                  ]
                    & map concat
                    & transpose
                    & map product
                    & maximum
        )

prevLtDist :: forall a. (Ord a, Bounded a) => [a] -> [Int]
prevLtDist = prevLtIdx <&> map (\(i, j) -> (i, abs $ i - j)) <&> sortOn fst <&> map snd

{- | For each index i, find the largest j < i st x_j > x_i.
 Runs in linear time.
-}
prevLtIdx :: forall a. (Ord a, Bounded a) => [a] -> [(Int, Int)]
prevLtIdx xs = go (zip [0 ..] xs) [(0, maxBound)] & flip execState []
  where
    go (x : xs) st = go xs =<< push x st
    go [] _ = pure ()

    -- Maintain a non-increasing stack.
    push (i, x) ((j, y) : st)
      | x > y = push (i, x) st
      | otherwise = modify ((i, j) :) $> ((i, x) : (j, y) : st)
    push x st = pure (x : st)

-- >>> part1
-- 1538

-- >>> part2
-- 496125
