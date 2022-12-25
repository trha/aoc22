{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Day5 where

import Control.Arrow ((>>>))
import Control.Monad.ST (ST)
import Control.Monad.ST.Strict (runST)
import Data.Char (isAsciiUpper)
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (break, foldl', inits, transpose, unfoldr)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V hiding (modify)
import qualified Data.Vector.Mutable as V

solve :: (T.Text -> T.Text) -> IO String
solve permute =
  T.readFile "src/Day5.txt"
    <&> ( T.lines
            >>> break T.null
            >>> ( \(stacks, insts) ->
                    ( stacks
                        & T.transpose
                        & map (T.filter isAsciiUpper)
                        & filter (not . T.null)
                        & V.fromList
                    , insts
                        & tail
                        & map
                          ( T.words
                              >>> map (T.unpack >>> read)
                              >>> \[_move, crateCount, _from, src, _to, dest] -> (crateCount :: Int, src - 1, dest - 1)
                          )
                    )
                )
            >>> ( \(stacks, insts) -> runST do
                    stacks <- V.unsafeThaw stacks
                    forM_ insts \(crateCount, i, j) -> do
                      src <- V.read stacks i
                      let (moved, remaining) = T.splitAt crateCount src
                      V.modify stacks (T.append (permute moved)) j
                      V.write stacks i remaining
                    V.unsafeFreeze stacks
                )
            >>> V.toList
            >>> map T.head
        )

part1 = solve T.reverse
part2 = solve id

-- >>> part1
-- "VWLCWGSDQ"

-- >>> part2
-- "TCGLQSLPW"
