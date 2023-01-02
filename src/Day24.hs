{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day24 where

import Control.Arrow ((>>>))
import qualified Control.Foldl as F
import Control.Monad (guard)
import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as A (UArray)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B (lines)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (mapAccumL)
import qualified Data.Set as S
import Data.Word (Word8)

type Blizzard = V2 (V2 Int)

pattern Wall, Ground, U, D, L, R :: Word8
pattern Wall = 35
pattern Ground = 46
pattern U = 94
pattern D = 118
pattern L = 60
pattern R = 62

default (Int)

solve n =
  B.readFile "src/Day24.txt"
    <&> ( B.lines
            >>> zipWith (\r -> zipWith (\c cell -> (V r c, cell)) [0 ..] . B.unpack) [0 ..]
            >>> concat
            >>> do
              (~(~(Just lr), ~(Just hr)), ~(~(Just lc), ~(Just hc)), blizzs) <-
                F.fold
                  ( (,,)
                      <$> F.premap (\(V r _, _) -> r) ((,) <$> F.minimum <*> F.maximum)
                      <*> F.premap (\(V _ c, _) -> c) ((,) <$> F.minimum <*> F.maximum)
                      <*> F.foldMap
                        ( \(p, cell) ->
                            V p <$> case cell of
                              U -> [V (-1) 0]
                              D -> [V 1 0]
                              L -> [V 0 (-1)]
                              R -> [V 0 1]
                              _ -> []
                        )
                        id
                  )
              ~(~(Just (s, _)), ~(Just (t, _))) <-
                F.fold
                  ( (,)
                      <$> F.find (\(V r _, cell) -> r == lr && cell == Ground)
                      <*> F.find (\(V r _, cell) -> r == hr && cell == Ground)
                  )
              let bfs' = bfs (V (V (lr + 1) (lc + 1)) (V (hr - 1) (hc - 1)))
              [(s, t), (t, s)]
                & cycle
                & take n
                & mapAccumL (\blizzs (s, t) -> bfs' s t blizzs) blizzs
                & snd
                & sum
                & pure
        )

part1 = solve 1
part2 = solve 3

-- >>> part1
-- 230

-- >>> part2
-- 713

bfs :: V2 (V2 Int) -> V2 Int -> V2 Int -> [Blizzard] -> ([Blizzard], Int)
bfs bnds s t = go 0 (S.singleton s)
  where
    go i (S.member t -> True) blizzs = (blizzs, i)
    go i q (step bnds -> blizzs) = go (i + 1) q' blizzs
      where
        q' = S.fromList do
          V r c <- S.toList q
          V dr dc <- adj5
          let (r', c') = (r + dr, c + dc)
          guard $ V r' c' == s || V r' c' == t || A.inRange (A.bounds free) (r', c') && free A.! (r', c')
          pure $ V r' c'
        free = A.accumArray @A.UArray (\_ x -> x) True (tup $ tup <$> bnds) $ map (\(V p _) -> (tup p, False)) blizzs

adj5 = [V 1 0, V 0 1, V (-1) 0, V 0 (-1), V 0 0]

step :: V2 (V2 Int) -> [Blizzard] -> [Blizzard]
step (V (V lr lc) (V hr hc)) =
  map
    ( \(V (V r c) d@(V dr dc)) ->
        let (r', c') = (r + dr, c + dc)
         in V (V (clamp r' lr hr) (clamp c' lc hc)) d
    )
  where
    clamp x lo hi
      | x < lo = hi
      | x > hi = lo
      | otherwise = x

data V2 a = V a a deriving (Show, Eq, Ord, Functor)

tup :: V2 a -> (a, a)
tup (V x y) = (x, y)
