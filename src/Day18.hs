{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Strict #-}

module Day18 where

import Control.Arrow ((>>>))
import qualified Data.ByteString.Char8 as B
import Data.Function (fix, (&))
import qualified Data.Set as S

default (Int)

part1 = do
  lns <- B.lines <$> B.readFile "src/Day18.txt"
  foldr
    ( \ln k cubes sides ->
        ln & B.split ',' & map B.readInt & \[Just (x, _), Just (y, _), Just (z, _)] ->
          let cube = P x y z
           in k (S.insert cube cubes) (adj6 cube ++ sides)
    )
    (\cubes -> length . filter (`S.notMember` cubes))
    lns
    S.empty
    []
    & pure

part2 = do
  lns <- B.lines <$> B.readFile "src/Day18.txt"
  foldr
    ( \ln k cubes lo hi sides ->
        ln & B.split ',' & map B.readInt & \[Just (x, _), Just (y, _), Just (z, _)] ->
          let cube = P x y z
           in k
                (S.insert cube cubes)
                (min <$> lo <*> (subtract 1 <$> cube))
                (max <$> hi <*> ((+ 1) <$> cube))
                (adj6 cube ++ sides)
    )
    ( \cubes lo hi ->
        let dfs seen [] = seen
            dfs seen (p : ps)
              | and ((<=) <$> lo <*> p)
                  && and ((<=) <$> p <*> hi)
                  && S.notMember p seen
                  && S.notMember p cubes =
                  dfs (S.insert p seen) (adj6 p ++ ps)
              | otherwise = dfs seen ps
            seen = dfs S.empty [lo]
         in length . filter (`S.member` seen)
    )
    lns
    S.empty
    (P maxBound maxBound maxBound)
    (P 0 0 0)
    []
    & pure

data Point a = P a a a deriving (Show, Eq, Ord, Foldable, Functor)
adj6 (P x y z) = [P (x + 1) y z, P (x - 1) y z, P x (y - 1) z, P x (y + 1) z, P x y (z - 1), P x y (z + 1)]

instance Applicative Point where
  pure x = P x x x
  (P fa fb fc) <*> (P a b c) = P (fa a) (fb b) (fc c)

--- >>> part1
-- 4636

-- >>> part2
-- 2572
