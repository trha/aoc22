{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
-- {-# OPTIONS_GHC -ddump-simpl
--     -dsuppress-idinfo
--     -dsuppress-coercions
--     -dsuppress-type-applications
--     -dsuppress-uniques
--     -dsuppress-module-prefixes #-}

module Day23 where

import Control.Arrow ((>>>))
import qualified Control.Foldl as F
import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.Extra (allM, anyM, firstJustM, ifM, notM, (&&^), (||^))
import Control.Monad.Loops (takeWhileM)
import Control.Monad.ST.Strict (ST, runST)
import qualified Data.Array.IArray as A
import qualified Data.Array.MArray as A
import qualified Data.Array.ST as A (STUArray)
import qualified Data.Array.Unboxed as A (UArray)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B (lines, unlines)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (groupBy, inits, tails)
import qualified Data.Map.Strict as M
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Word (Word8)

default (Int)

pattern Elf, Ground :: Word8
pattern Elf = 35
pattern Ground = 46

type Coord = (Int, Int)
type Grid a = a Coord Word8

grid =
  B.readFile "src/Day23.txt"
    <&> ( B.lines
            >>> do
              r0 <- length
              c0 <- B.length <$> head
              gridList <- concat <$> zipWith (\r row -> zipWith (\c cell -> ((r, c), cell)) [c0 ..] (B.unpack row)) [r0 ..]
              pure $ A.accumArray @A.UArray (\_ x -> x) Ground ((0, 0), (r0 * 3, c0 * 3)) gridList
        )

part1 = do
  grid <- grid
  pure $ runST do
    grid <- A.thaw grid
    looks & map (step grid) & take 10 & sequence_
    bnds <- A.getBounds grid
    (r1, c1, r2, c2, n) <-
      foldM
        ( \st@(!r1, !c1, !r2, !c2, !n) rc@(r, c) -> do
            cell <- A.readArray grid rc
            pure
              if cell == Elf
                then (min r1 r, min c1 c, max r2 r, max c2 c, n + 1)
                else st
        )
        (maxBound, maxBound, minBound, minBound, 0)
        (A.range bnds)
    pure $ (r2 - r1 + 1) * (c2 - c1 + 1) - n

part2 = do
  grid <- grid
  pure $ runST do
    grid <- A.thaw grid
    succ . length <$> takeWhileM (step grid) looks

-- >>> part1
-- 4195

-- >>> part2
-- 1069

showGrid :: Grid A.UArray -> B.ByteString
showGrid g = g & A.assocs & groupBy ((==) `on` (fst . fst)) & map (map snd >>> B.pack) & B.unlines

step :: forall s. Grid (A.STUArray s) -> [([Coord], Coord)] -> ST s Bool
step g looks = do
  bnds <- A.getBounds g
  
  let canMove rc@(r, c) =
        (A.readArray g rc <&> (== Elf))
          &&^ anyM
            ( \(dr, dc) ->
                let rc' = (r + dr, c + dc)
                 in pure (A.inRange bnds rc') &&^ (A.readArray g rc' <&> (== Elf))
            )
            adj8

      propose rc@(r, c) =
        firstJustM
          ( \(ds, (dest_dr, dest_dc)) -> do
              free <-
                allM
                  ( \(dr, dc) ->
                      let rc' = (r + dr, c + dc)
                       in notM (pure $ A.inRange bnds rc') ||^ (A.readArray g rc' <&> (/= Elf))
                  )
                  ds
              pure if free then Just ((r + dest_dr, c + dest_dc), L1 rc) else Nothing
          )
          looks

  dests <-
    flip F.foldM (A.range bnds)
      . prefilterMapM (ifM <$> canMove <*> propose <*> const (pure Nothing))
      . F.generalize
      $ F.Fold (\m (k, v) -> M.insertWith (<>) k v m) M.empty id

  changed <- newSTRef False
  forM_ (M.toList dests) \case
    (rc', L1 rc) -> do
      (a, b) <- (,) <$> A.readArray g rc <*> A.readArray g rc'
      A.writeArray g rc b
      A.writeArray g rc' a
      writeSTRef changed True
    _ -> pure ()
  readSTRef changed

prefilterMapM :: (Monad m) => (a -> m (Maybe b)) -> F.FoldM m b r -> F.FoldM m a r
prefilterMapM f (F.FoldM step begin done) = F.FoldM step' begin done
 where
  step' x a =
    f a >>= \case
      Just b -> step x b
      Nothing -> pure x

data LMax2 a = L1 a | L2 deriving (Show)
instance Semigroup (LMax2 a) where
  L1 _ <> _ = L2
  L2 <> _ = L2

looks =
  [ ([(-1, 0), (-1, 1), (-1, -1)], (-1, 0))
  , ([(1, 0), (1, 1), (1, -1)], (1, 0))
  , ([(0, -1), (-1, -1), (1, -1)], (0, -1))
  , ([(0, 1), (-1, 1), (1, 1)], (0, 1))
  ]
    & zipWith (++) <$> tails <*> inits
    & init
    & cycle

adj8 = [(dr, dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], not $ dr == 0 && dc == 0]

