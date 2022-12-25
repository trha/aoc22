{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((>>>))
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (findIndices, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Packet = PInt Int | PList [Packet] deriving (Show, Read)

parsePacket :: String -> Packet
parsePacket =
  zip <$> ('_' :) <*> id
    >>> concatMap \case
      (_, '[') -> "PList ["
      (p, c) | isDigit c && not (isDigit p) -> "PInt " ++ [c]
      (_, c) -> [c]
    >>> read

instance Eq Packet where
  a == b = compare a b == EQ

instance Ord Packet where
  compare (PInt a) (PInt b) = compare a b
  compare (PList xs) (PList ys) = compare xs ys
  compare a@(PInt _) ys@(PList _) = compare (PList [a]) ys
  compare xs@(PList _) b@(PInt _) = compare xs (PList [b])

part1 =
  T.readFile "src/Day13.txt"
    <&> ( T.splitOn "\n\n"
            >>> map (T.lines >>> map (T.unpack >>> parsePacket) >>> (\[a, b] -> fromEnum $ a <= b))
            >>> zipWith (*) [1 :: Int ..]
            >>> sum
        )

part2 =
  T.readFile "src/Day13.txt"
    <&> ( T.lines
            >>> filter (not . T.null)
            >>> map (T.unpack >>> parsePacket)
            >>> (divs ++)
            >>> sort
            >>> findIndices (`elem` divs)
            >>> map (+ 1)
            >>> product
        )
  where
    divs = [PList [PList [PInt 2]], PList [PList [PInt 6]]]

-- >>> part1
-- 5675

-- >>> part2
-- 20383
