module Day3 where

import Control.Arrow ((>>>))
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (groupBy, intersect)
import qualified Data.ByteString.Char8 as B
import Optics

solve :: (B.ByteString -> [[B.ByteString]]) -> IO Int
solve toGroups =
  B.readFile "src/Day3.txt"
    <&> ( toGroups
            >>> map
              ( map B.unpack
                  >>> foldl1 intersect
                  >>> head
                  >>> \x -> (fromEnum x - 38) `rem` 58
              )
            >>> sum
        )

-- >>> part1
-- 8018

-- >>> part2
-- 2518

part1 :: IO Int
part1 = solve $ B.lines >>> map ((B.length >>> (`quot` 2)) >>= B.splitAt <&> toListOf both)

part2 :: IO Int
part2 = solve $ B.lines >>> chunksOf 3

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = zip ([0 ..] >>= replicate n) >>> groupBy ((==) `on` fst) >>> map (map snd)

