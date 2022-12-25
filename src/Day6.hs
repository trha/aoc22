module Day6 where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.IntMap.Strict as M
import Data.List (findIndex, scanl')
import Data.Word (Word32)
import Numeric (showBin)

solve :: Int -> IO (Maybe Int)
solve n =
  readFile "src/Day6.txt"
    <&> ( map fromEnum
            >>> (zip =<< (replicate n 0 ++))
            >>> scanl'
              ( \m (prev, cur) ->
                  m
                    & M.update (subtract 1 >>> ensure (> 0)) prev
                    & M.insertWith (+) cur 1
              )
              M.empty
            >>> findIndex ((== n) . M.size)
        )

part1 = solve 4
part2 = solve 14

ensure :: (a -> Bool) -> a -> Maybe a
ensure f x
  | f x = Just x
  | otherwise = Nothing

-- >>> part1
-- Just 1300

-- >>> part2
-- Just 3986
