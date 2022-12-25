module Day3 where

import Control.Arrow ((>>>))
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (groupBy, intersect)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Optics

solve :: (T.Text -> [[T.Text]]) -> IO Int
solve toGroups =
  T.readFile "src/Day3.txt"
    <&> ( toGroups
            >>> map
              ( map T.unpack
                  >>> foldl1 intersect
                  >>> head
                  >>> \x -> (fromEnum x - 38) `rem` 58
              )
            >>> sum
        )

part1 :: IO Int
part1 = solve $ T.lines >>> map ((T.length >>> (`quot` 2)) >>= T.splitAt <&> toListOf both)

part2 :: IO Int
part2 = solve $ T.lines >>> chunksOf 3

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = zip ([0 ..] >>= replicate n) >>> groupBy ((==) `on` fst) >>> map (map snd)

-- >>> part1
-- 8018

-- >>> part2
-- 2518
