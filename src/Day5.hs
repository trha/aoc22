{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Day5 where

import Control.Arrow ((>>>))
import Control.Monad.ST (ST)
import Control.Monad.ST.Strict (runST)
import Data.Char (isAsciiUpper)
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (break, foldl', inits, transpose, unfoldr)
import qualified Data.Vector as V hiding (modify)
import qualified Data.Vector.Mutable as V
import qualified Data.ByteString.Char8 as B

solve permute =
  B.readFile "src/Day5.txt"
    <&> ( B.lines
            >>> break B.null
            >>> ( \(stacks, insts) ->
                    ( stacks
                        & B.transpose
                        & map (B.filter isAsciiUpper)
                        & filter (not . B.null)
                        & V.fromList
                    , insts
                        & tail
                        & map
                          ( B.words
                              >>> map (B.readInt >>> (\(Just (n, _)) -> n))
                              >>> \[_move, crateCount, _from, src, _to, dest] -> (crateCount :: Int, src - 1, dest - 1)
                          )
                    )
                )
            >>> ( \(stacks, insts) -> runST do
                    stacks <- V.unsafeThaw stacks
                    forM_ insts \(crateCount, i, j) -> do
                      src <- V.read stacks i
                      let (moved, remaining) = B.splitAt crateCount src
                      V.modify stacks (B.append (permute moved)) j
                      V.write stacks i remaining
                    V.unsafeFreeze stacks
                )
            >>> V.toList
            >>> map B.head
        )

part1 = solve B.reverse
part2 = solve id

-- >>> part1
-- "VWLCWGSDQ"

-- >>> part2
-- "TCGLQSLPW"
