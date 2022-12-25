{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Day12 where

import Data.Array ((!))
import qualified Data.Array as A
import Data.Function ((&))
import qualified Data.IntSet as IS
import Data.List (find, mapAccumL)
import Data.Maybe (catMaybes, isJust, mapMaybe)

solve :: (Char -> Bool) -> IO Int
solve canStartAt = do
  input <- readFile "src/Day12.txt"
  let lns = lines input
      g = A.listArray ((1, 1), (length lns, length (head lns))) $ filter (/= '\n') input
      bnds = A.bounds g
      neighborsOf u@(i, j) =
        filter
          (\v -> A.inRange bnds v && (val (g ! v) - val (g ! u) < 2))
          [(i + 1, j), (i, j + 1), (i - 1, j), (i, j - 1)]
      val x = fromEnum case x of
        'S' -> 'a'
        'E' -> 'z'
        x -> x
      bfs seen q
        | null unseen = []
        | otherwise =
            unseen
              ++ bfs
                seen'
                (concatMap (\(u, d) -> neighborsOf u & map (,d + 1)) unseen)
        where
          (seen', catMaybes -> unseen) =
            mapAccumL
              ( \seen (u, d) ->
                  let u' = bnds `A.index` u
                   in if u' `IS.member` seen
                        then (seen, Nothing)
                        else (IS.insert u' seen, Just (u, d))
              )
              seen
              q
  A.assocs g
    & mapMaybe (\(u, e) -> if canStartAt e then Just (u, 0) else Nothing)
    & bfs IS.empty
    & find (\(u, _) -> g ! u == 'E')
    & (\(Just (_, d)) -> d)
    & pure

part1 = solve (== 'S')
part2 = solve (`elem` "Sa")

-- >>> part1
-- [((21,59),350)]
--
-- >>> part2
-- [((21,59),349)]
