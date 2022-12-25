{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Day15 where

import Control.Arrow ((>>>))
import qualified Control.Foldl as F
import Data.Array (Ix (inRange))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl', scanl', sortOn)
import Data.Maybe (mapMaybe)
import Data.Semigroup (Any (..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

inp =
  T.readFile "src/Day15.txt"
    <&> ( T.lines
            >>> map
              ( T.split (`elem` "=,:")
                  >>> map (T.unpack >>> read)
                  >>> \[_, sx, _, sy, _, bx, _, by] -> (sx, sy, bx, by :: Int)
              )
        )

part1 =
  inp
    <&> ( map
            ( \(sx, sy, bx, by) ->
                let d = abs (bx - sx) + abs (by - sy)
                 in ( [sx - d, sx + d]
                    , \(x, y) -> Any $ x /= bx && y /= by && abs (x - sx) + abs (y - sy) <= d
                    )
            )
            >>> unzip
            >>> \( F.fold ((,) <$> F.minimum <*> F.maximum) . concat -> (Just xMin, Just xMax)
                  , mconcat -> isFree
                  ) -> [xMin .. xMax] & filter (\x -> getAny $ isFree (x, 2000000)) & length
        )

part2 = do
  sensors <- inp <&> map (\(sx, sy, bx, by) -> (sx, sy, (bx - sx) + abs (by - sy)))
  pure $
    [lo .. hi]
      & map
        ( \x ->
            ( x
            , sensors
                & mapMaybe
                  ( \(sx, sy, d) ->
                      let y = if x >= sx then d + sx + sy - x else d + x + sy - sx
                       in if sx - d <= x && x <= sx + d then Just (2 * sy - y, y) else Nothing
                  )
                & sortOn fst
                & foldl' (\y (l, r) -> if y >= l then max y $ r + 1 else y) 0
            )
        )
      & filter (inRange rng . snd)
      & \[(x, y)] -> x * hi + y
  where
    rng@(lo, hi) = (0, 4000000)

-- >>> part1
-- 4679273

-- >>> part2
-- 10884459367718
