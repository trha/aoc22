{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T

part1 :: IO Int
part1 = T.readFile "src/Day1.txt" <&> (T.splitOn "\n\n" >>> map (T.lines >>> map (T.unpack >>> read) >>> sum) >>> maximum)

part2 :: IO Int
part2 = T.readFile "src/Day1.txt" <&> (T.splitOn "\n\n" >>> map (T.lines >>> map (T.unpack >>> read) >>> sum) >>> sortOn Down >>> take 3 >>> sum)

-- >>> part1
-- 69528

--- >>> part2
-- 206152
