{-# LANGUAGE BlockArguments #-}

module Day20 where

import Control.Arrow ((>>>))
import Control.Monad (forM_, replicateM_, when)
import Control.Monad.Trans.Cont (ContT (ContT), runContT)
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as V (fromList, unsafeThaw)
import qualified Data.Vector.Unboxed.Mutable as V

solve key rep = do
  xs <- B.readFile "src/Day20.txt" >>= (B.lines >>> map (B.readInt >>> \(Just (x, _)) -> x * key) >>> zip [0 :: Int ..] >>> V.fromList >>> V.unsafeThaw)
  let n = V.length xs
  replicateM_ rep do
    forM_ [0 .. n - 1] \i -> do
      (j, x) <- mifind xs (\j (i', x) -> if i' == i then Just (j, x) else Nothing)
      when (x /= 0) do
        let j' = (j + x) `mod` (n - 1)
            d = signum $ j' - j
        forM_ [j, j + d .. j' - d] \z -> V.swap xs z (z + d)
  i0 <- mifind xs (\i (_, x) -> if x == 0 then Just i else Nothing)
  sum <$> sequence [snd <$> V.read xs ((i0 + d) `rem` n) | d <- [1000, 2000, 3000]]

part1 = solve 1 1
part2 = solve 811589153 10

-- >>> part1
-- 2622

-- >>> part2
-- 1538773034088

mifind :: (V.PrimMonad m, V.Unbox a) => V.MVector (V.PrimState m) a -> (Int -> a -> Maybe b) -> m b
mifind v f =
  flip runContT undefined $
    V.ifoldM'
      ( \() i x -> case f i x of
          Just y -> ContT $ const $ pure y
          Nothing -> pure ()
      )
      ()
      v
