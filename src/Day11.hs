{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day11 where

import Control.Arrow ((>>>))
import Control.Monad (replicateM_)
import Data.Bool (bool)
import Data.Foldable (forM_)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V (Vector, fromList, toList, unsafeFreeze, unsafeThaw)
import qualified Data.Vector.Mutable as V (iforM_, length, modify, replicate)
import qualified Data.Vector.Unboxed as VU (Vector, freeze, fromList, toList, unsafeFreeze, unsafeThaw)
import qualified Data.Vector.Unboxed.Mutable as VU (MVector, forM_, modify, replicate)
import GHC.Generics (Generic)
import GHC.List (foldl1')
import Optics

data Monkey = MkMonkey
  { items :: ~[Int]
  , operation :: Int -> Int
  , divisor :: Int
  , ifTrue :: Int
  , ifFalse :: Int
  }
  deriving (Generic)

solve iterationCount alterItem = do
  inp <- T.readFile "src/Day11.txt"
  let monkeys =
        inp
          & T.splitOn "\n\n"
          & map
            ( \( T.lines ->
                  [ _monkey_n
                    , T.splitOn ": " -> (!! 1) >>> T.splitOn ", " >>> map (read @Int . T.unpack) -> !items
                    , T.words >>> reverse -> (opR : op : opL : _)
                    , T.words >>> last >>> read @Int . T.unpack -> divisor
                    , T.words >>> last >>> read @Int . T.unpack -> ifTrue
                    , T.words >>> last >>> read @Int . T.unpack -> ifFalse
                    ]
                ) ->
                  MkMonkey
                    { items
                    , operation =
                        let f = case op of
                              "+" -> (+)
                              "*" -> (*)
                            getL = case opL of
                              "old" -> id
                              x -> const $ read $ T.unpack x
                            getR = case opR of
                              "old" -> id
                              x -> const $ read $ T.unpack x
                         in \o -> f (getL o) (getR o) `rem` base
                    , divisor
                    , ifTrue
                    , ifFalse
                    }
            )
      base = foldl1' lcm $ map (^. #divisor) monkeys

  monkeys <- V.unsafeThaw $ V.fromList monkeys
  inspects <- VU.replicate (V.length monkeys) (0 :: Int)

  replicateM_ iterationCount $ V.iforM_ monkeys \i MkMonkey{items, operation, divisor, ifTrue, ifFalse} -> do
    V.modify monkeys (\m -> m{items = []}) i
    forM_ items \(alterItem . operation -> item) -> do
      VU.modify inspects succ i
      V.modify monkeys (\m -> m & #items %~ (++ [item])) $ bool ifFalse ifTrue $ item `rem` divisor == 0

  product . take 2 . sortOn Down . VU.toList <$> VU.unsafeFreeze inspects

part1 = solve 20 (`quot` 3)
part2 = solve 10000 id

-- >>> part1
-- 56120

-- >>> part2
-- 24389045529
