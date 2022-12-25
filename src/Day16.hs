{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day16 where

import Control.Arrow ((>>>))
import Control.Monad (foldM, forM_)
import Control.Monad.ST.Strict (ST, runST)
import qualified Data.Array.Base as A
import Data.Array.IArray (Array, (!))
import qualified Data.Array.IArray as A
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Bits (Bits (bit, setBit, xor))
import Data.Char (isAsciiUpper)
import Data.Functor ((<&>))
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Read (readMaybe)

solve extract =
  T.readFile "src/Day16.txt"
    <&> ( T.lines
            >>> map
              ( T.split (`elem` (" ;,=" :: String))
                  >>> filter (\x -> (T.length x == 2 && T.all isAsciiUpper x) || isJust (readMaybe @Int (T.unpack x)))
                  >>> (\(u : (read @Int . T.unpack -> r) : vs) -> (u, r, vs))
              )
            >>> sortOn (\(_, r, _) -> Down r)
            >>> zip [0 ..]
            >>> ( \inp ->
                    foldr
                      ( \(i, (u, r, vs)) k m ->
                          let (m', g, rs) = k $ M.insert u i m in (m', map (m' M.!) vs : g, r : rs)
                      )
                      (,[],[])
                      inp
                      M.empty
                )
            >>> ( \(m, A.listArray (0, M.size m - 1) -> g, A.listArray (0, M.size m - 1) -> rs) ->
                    extract (m M.! "AA") $ runST $ build g rs 30
                )
        )

part_1_2 = solve \u tbl@(A.bounds -> (_, (_, _, bb))) ->
  ( tbl ! (30, u, 0)
  , maximum [tbl ! (26, u, o) + tbl ! (26, u, xor bb o) | o <- [0 .. bb]]
  )

-- >>> part_1_2
-- (2080,2752)
-- (2080,2752)

build :: forall s. Array Int [Int] -> UArray Int Int -> Int -> ST s (UArray (Int, Int, Int) Int)
build g rs tt = do
  tbl <- A.newArray @(STUArray s) @Int ((0, 0, 0), (tt, n, bb)) 0
  forM_ ((,,) <$> [1 .. tt] <*> [0 .. n] <*> [0 .. bb]) \(t, u, opened) ->
    A.writeArray tbl (t, u, opened)
      =<< foldM
        (\(!mx) x -> max mx <$> x)
        0
        ( [ (+ (rs ! u * (t - 1)))
            <$> A.readArray tbl (t - 1, u, opened')
          | let opened' = setBit opened u
          , opened' /= opened && rs ! u /= 0
          ]
            ++ [A.readArray tbl (t - 1, v, opened) | v <- g ! u]
        )
  A.unsafeFreeze tbl
  where
    bb = pred $ bit $ length $ takeWhile (/= 0) $ A.elems rs
    n = A.numElements rs - 1
