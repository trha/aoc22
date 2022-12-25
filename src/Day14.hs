{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Day14 where

import Control.Arrow ((>>>))
import qualified Control.Foldl as F
import Control.Monad.ST.Strict (ST, runST)
import qualified Data.Array.IArray as A
import qualified Data.Array.MArray as A
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unsafe as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T

default (Int)

data OnWall = OnWallGrow | OnWallStop

solve onWall = do
  inp <- T.readFile "src/Day14.txt"
  let endPoints =
        inp
          & T.lines
          & map
            ( T.splitOn " -> "
                >>> map (T.splitOn "," >>> map (T.unpack >>> read) >>> \[x, y] -> (x, y))
            )

      (xMin0, yMin0, xMax0, yMax0) =
        F.fold
          ( (,,,)
              <$> (fromJust <$> F.premap fst F.minimum)
              <*> (fromJust <$> F.premap snd F.minimum)
              <*> (fromJust <$> F.premap fst F.maximum)
              <*> (fromJust <$> F.premap snd F.maximum)
          )
          ((500, 0) : concat endPoints)

      simulate :: forall s. Int -> ST s Int
      simulate !xtra = do
        let bnds@((xMin, yMin), (xMax, yMax)) = ((xMin0 - xtra, yMin0), (xMax0 + xtra, yMax0 + 2))
        g <-
          A.unsafeThaw @_ @UArray @_ @(STUArray s) $
            A.accumArray
              (\_ x -> x)
              '.'
              ((xMin, yMin), (xMax, yMax))
              ( concat [[((xMin, y), '|'), ((xMax, y), '|')] | y <- [yMin .. yMax]] -- Walls.
                  ++ [((x, yMax), '#') | x <- [xMin .. xMax]] -- Final platform.
                  ++ ( endPoints
                        & concatMap
                          ( zipWith
                              (\(x1, y1) (x2, y2) -> (\x y -> ((x, y), '#')) <$> [min x1 x2 .. max x1 x2] <*> [min y1 y2 .. max y1 y2])
                              <$> id
                              <*> tail
                          )
                        & concat
                     )
              )

        let go (x, y) !i k =
              A.readArray g (x, y) >>= \case
                '.' -> go (x, y + 1) i $
                  \i b1 -> go (x - 1, y + 1) i $
                    \i b2 -> go (x + 1, y + 1) i $
                      \i b3 ->
                        if b1 && b2 && b3
                          then A.writeArray g (x, y) 'o' *> k (i + 1) True
                          else k i False
                '|' -> case onWall of
                  OnWallStop -> pure i
                  OnWallGrow -> simulate (xtra * 2) -- Hit wall, grow bounds exponentially.
                _ -> k i True

        go (500, 0) 0 (\i _ -> pure i)

  pure $ runST $ simulate 1

part1 = solve OnWallStop
part2 = solve OnWallGrow

-- >>> part1
-- 696

-- >>> part2
-- 23610
