{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day22 where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Foldl.ByteString (Word8)
import Control.Monad (guard, when)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), hoistMaybe)
import Control.Monad.Trans.State.Strict (State, execState, get, gets, put)
import qualified Data.Array as A (Array)
import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as A (UArray)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B (lines, readInt)
import Data.Function ((&))
import Data.List (find, group, mapAccumL)
import qualified Data.Map.Strict as M

pattern Pit, Wall, Ground :: Word8
pattern Pit = 32
pattern Wall = 35
pattern Ground = 46

pattern L, R :: Int
pattern L = -76
pattern R = -82

type Grid a e = a (Int, Int) e
data Edge = ER | EB | EL | ET deriving (Eq, Ord, Show, Enum, Bounded)
type Side = Int

solve warp0 = do
  (inpMap, B.drop 2 -> inpPath) <- B.breakSubstring "\n\n" <$> B.readFile "src/Day22.txt"
  let
    path = parse inpPath
      where
        parse (B.readInt -> Just (n, b)) = n : parse b
        parse (B.uncons -> Just (d, b)) = -fromEnum d : parse b
        parse _ = []
    rows = B.lines inpMap
    (nr, nc) = (length rows, maximum (map B.length rows))
    g =
      A.accumArray @A.UArray
        (\_ x -> x)
        Pit
        ((0, 0), (nr - 1, nc - 1))
        [((r, c), m) | (r, row) <- zip [0 ..] rows, (c, m) <- zip [0 ..] $ B.unpack row]

    warp = warp0 g nr nc
    follow d (r, c) [] = (r + 1) * 1000 + 4 * (c + 1) + fromEnum d
    follow d p (L : path) = follow (ccw d) p path
    follow d p (R : path) = follow (cw d) p path
    follow d p (0 : path) = follow d p path
    follow d p (n : path)
      | Wall <- g A.! p' = follow d p path
      | otherwise = follow d' p' $ (n - 1) : path
      where
        (d', p') = warp d p

  pure $ follow ER (head [(0, c) | c <- [0 .. nc - 1], g A.! (0, c) /= Pit]) path

-- >>> part1
-- 136054

-- >>> part2
-- 122153

part1 = solve $ \g nr nc d@(edgeToDir -> (dr, dc)) (r, c) ->
  (d, head [rc | rc <- tail $ iterate (\(r, c) -> ((r + dr) `mod` nr, (c + dc) `mod` nc)) (r, c), g A.! rc /= Pit])

part2 = solve $ \g nr nc ->
  let
    (sideLen, net) = netOf g
    links = linkSides net

    sideCoord side = head [rc | (rc, s) <- A.assocs net, s == side]
    sideOfCoord (r, c) = net A.! (r `quot` sideLen, c `quot` sideLen)
    globalToLocal side (r, c) = let (sr, sc) = sideCoord side in (r - sr * sideLen, c - sc * sideLen)
    localToGlobal side (r, c) = let (sr, sc) = sideCoord side in (r + sr * sideLen, c + sc * sideLen)
    oob (r, c) = r < 0 || r >= sideLen || c < 0 || c >= sideLen
    wrap (r, c) = (r `mod` sideLen, c `mod` sideLen)
   in
    \d p ->
      let
        (side, edge) = (sideOfCoord p, d)
        (side', edge') = links M.! (side, d)
        p' = p `along` d
        lp = globalToLocal side p
        lp' = globalToLocal side p'
        d' = warpDir edge edge' d
       in
        if oob lp'
          then (d', localToGlobal side' $ wrap $ warpCoord sideLen edge edge' lp `along` d')
          else (d, p')

netOf :: A.IArray a Word8 => Grid a Word8 -> (Int, Grid A.UArray Side)
netOf g =
  [ [g A.! (r, c) /= Pit | c <- [cl, cl + side .. ch]]
  | r <- [rl .. rh]
  ]
    & group
    & map head
    & ( \net ->
          A.listArray ((0, 0), (length net - 1, length (head net) - 1))
            . snd
            . mapAccumL (\i s -> if s then (i + 1, i) else (i, 0)) 1
            $ concat net
      )
    & (side,)
  where
    ((rl, cl), (rh, ch)) = A.bounds g
    side = sideLengthOf g

sideLengthOf :: A.IArray a Word8 => Grid a Word8 -> Int
sideLengthOf = A.elems >>> filter (/= Pit) >>> length >>> \n -> intSqrt $ n `quot` 6
  where
    intSqrt n = last $ takeWhile (\i -> i * i <= n) [1 ..]

linkSides :: A.IArray a Side => Grid a Side -> M.Map (Side, Edge) (Side, Edge)
linkSides net = execState (fixState link) obvious
  where
    -- Obvious links from the net.
    obvious = M.fromList do
      ((r, c), side) <- A.assocs net & filter ((> 0) . snd)
      edge <- allEdges
      let
        (dr, dc) = edgeToDir edge
        (r', c') = (r + dr, c + dc)
      guard $ A.inRange (A.bounds net) (r', c')
      let side' = net A.! (r', c')
      concat $ [mirror ((side, edge), (side', opp edge)) | side' > 0]

    link =
      sequence_
        [ get
          >>= M.alterF
            (\existing -> runMaybeT $ hoistMaybe existing <|> trace cw side edge <|> trace ccw side edge)
            (side, edge)
          >>= put
        | side <- [1 .. 6]
        , edge <- allEdges
        ]

    -- Trace the side that shares the `edge` of `side` with `side`.
    trace rot side edge = do
      (side', edge') <- MaybeT $ gets $ M.lookup (side, rot edge)
      (side'', edge'') <- MaybeT $ gets $ M.lookup (side', rot edge')
      pure (side'', rot edge'')

-- | Find the fixpoint of a state-modifying action.
fixState :: Eq s => State s () -> State s ()
fixState act = go
  where
    go = do
      before <- get
      act
      after <- get
      when (before /= after) go

allEdges :: [Edge]
allEdges = [minBound .. maxBound]

edgeToDir :: Edge -> (Int, Int)
edgeToDir = \case
  ET -> (-1, 0)
  ER -> (0, 1)
  EB -> (1, 0)
  EL -> (0, -1)

warpCoord
  :: Int
  -- ^ Side length
  -> Edge
  -- ^ From edge
  -> Edge
  -- ^ To edge
  -> (Int, Int)
  -- ^ From side-local coordinate
  -> (Int, Int)
  -- ^ To side-local coordiate
warpCoord s a b (r, c) = case fromEnum a - fromEnum b of
  0 -> (s - 1 - r, s - 1 - c)
  3 -> (s - 1 - c, r)
  -3 -> (c, s - 1 - r)
  2 -> (r, c)
  -2 -> (r, c)
  1 -> (c, s - 1 - r)
  -1 -> (s - 1 - c, r)

warpDir :: Edge -> Edge -> Edge -> Edge
warpDir a b = case fromEnum a - fromEnum b of
  0 -> opp
  (-3) -> cw
  3 -> ccw
  (-2) -> id
  2 -> id
  (-1) -> ccw
  1 -> cw

along :: (Int, Int) -> Edge -> (Int, Int)
along (r, c) d = let (dr, dc) = edgeToDir d in (r + dr, c + dc)

mirror (a, b) = [(a, b), (b, a)]

opp = cw . cw

cw :: forall a. (Enum a, Bounded a) => a -> a
cw x = toEnum $ lo + (fromEnum x + 1) `rem` (hi - lo + 1)
  where
    (lo, hi) = (fromEnum @a minBound, fromEnum @a maxBound)

ccw :: forall a. (Enum a, Bounded a) => a -> a
ccw x = toEnum $ lo + (fromEnum x - 1) `mod` (hi - lo + 1)
  where
    (lo, hi) = (fromEnum @a minBound, fromEnum @a maxBound)
