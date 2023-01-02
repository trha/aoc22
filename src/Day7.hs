{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow ((>>>))
import qualified Data.ByteString.Char8 as B
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (tails, unfoldr)
import qualified Data.Map.Strict as Map

dirSizes :: IO [Int]
dirSizes =
  B.readFile "src/Day7.txt"
    <&> ( B.words
            >>> ([],)
            >>> unfoldr \case
              (_ : cwd, "$" : "cd" : ".." : rest) -> Just ([], (cwd, rest))
              (_, "$" : "cd" : "/" : rest) -> Just ([], ([], rest))
              (cwd, "$" : "cd" : dir : rest) -> Just ([], (dir : cwd, rest))
              (cwd, "$" : "ls" : rest) -> Just ([], (cwd, rest))
              (cwd, "dir" : _dir : rest) -> Just ([], (cwd, rest))
              (cwd, size : _file : rest) -> Just (tails cwd & map (,B.readInt size & (\(Just (n, _)) -> n)), (cwd, rest))
              (_, []) -> Nothing
            >>> concat
            >>> Map.fromListWith (+)
            >>> Map.elems
        )

part1 = dirSizes <&> (filter (<= 100000) >>> sum)
part2 =
  dirSizes <&> do
    used <- head
    filter ((<= 70000000 - 30000000) . (used -)) <&> minimum

-- >>> part1
-- 1582412

-- >>> part2
-- 3696336
