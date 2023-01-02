{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

module Day19 where

import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.Trans.State.Strict (State, execState, get, modify')
import Control.Parallel.Strategies (parMap, rseq)
import qualified Data.ByteString.Char8 as B
import Data.Function ((&))
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Optics ((.~), (^.))

data V4 a = V {ore :: a, clay :: a, obsidian :: a, geode :: a}
  deriving (Generic, Show, Functor, Foldable, Traversable)

instance Applicative V4 where
  pure x = V x x x x
  V fa fb fc fd <*> V a b c d = V (fa a) (fb b) (fc c) (fd d)

qualities t =
  B.readFile "src/Day19.txt"
    <&> ( B.lines
            >>> parMap
              rseq
              ( ints
                  >>> \[ bpId
                        , oreRobotOreCost
                        , clayRobotOreCost
                        , obsidianRobotOreCost
                        , obsidianRobotClayCost
                        , geodeRobotOreCost
                        , geodeRobotObsidianCost
                        ] ->
                      ( bpId
                      , dfs
                          ( V
                              (V oreRobotOreCost 0 0 0)
                              (V clayRobotOreCost 0 0 0)
                              (V obsidianRobotOreCost obsidianRobotClayCost 0 0)
                              (V geodeRobotOreCost 0 geodeRobotObsidianCost 0)
                          )
                          t
                          (pure 0)
                          (pure 0 & #ore .~ 1)
                      )
              )
        )

part1 = sum . map (uncurry (*)) <$> qualities 24
part2 = product . map snd . take 3 <$> qualities 32

--- >>> part1
-- 1466

--- >>> part2
-- 8250

dfs :: V4 (V4 Int) -> Int -> V4 Int -> V4 Int -> Int
dfs bp t rocks robots = execState (go t rocks robots) 0
  where
    his = maximum <$> sequenceA bp & #geode .~ maxBound

    go 0 rocks _ = modify' $ max (rocks ^. #geode)
    go t rocks robots = do
      modify' $ max $ robots ^. #geode * t + rocks ^. #geode
      gmax <- get
      when (t * (t - 1) `quot` 2 + robots ^. #geode * t + rocks ^. #geode > gmax) $
        sequence_ $
          ( \required delta robot_i hi_i ->
              if
                  | robot_i < hi_i
                  , Just t' <-
                      fmap maximum . sequence $
                        ( \req rok robo ->
                            if req > rok
                              then (req - rok + robo - 1) `safeQuot` robo
                              else Just 0
                        )
                          <$> required
                          <*> rocks
                          <*> robots
                  , t' < t ->
                      go
                        (t - t' - 1)
                        ((\rok robo req -> rok + (t' + 1) * robo - req) <$> rocks <*> robots <*> required)
                        ((+) <$> delta <*> robots)
                  | otherwise -> pure ()
          )
            <$> bp
            <*> V (V 1 0 0 0) (V 0 1 0 0) (V 0 0 1 0) (V 0 0 0 1)
            <*> robots
            <*> his

ints b
  | B.null b = []
  | Just (i, b') <- B.readInt b = i : ints b'
  | otherwise = ints $ B.tail b

safeQuot _ 0 = Nothing
safeQuot a b = Just $ a `quot` b
