{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Day17 where

import Control.Arrow ((>>>))
import qualified Control.Foldl as F
import Control.Monad (foldM)
import Data.Array.Base (UArray (UArray))
import qualified Data.Array.IArray as A
import Data.Bits (Bits (clearBit, setBit, testBit))
import Data.Bool (bool)
import Data.Functor ((<&>))
import qualified Data.IntMap as M
import Data.List (foldl', scanl')
import Data.Maybe (fromJust, isJust, isNothing)
import GHC.Generics (Generic)
import Optics (A_Lens, Index, IxValue, Ixed (..), lens, (&), (.~), (<&>), (^.))

pieces =
  [ MkPiece 4 1 0b1111
  , MkPiece 3 3 0b010111010
  , MkPiece 3 3 0b100100111
  , MkPiece 1 4 0b1111
  , MkPiece 2 2 0b1111
  ]

field0 = MkPiece 7 0 0

part1 = do
  ds <- readFile "src/Day17.txt"
  plays (take 2022 $ cycle pieces) field0 (cycle ds) & last & fst & height & pure

part2 = do
  ds <- readFile "src/Day17.txt"
  plays (take n0 $ cycle pieces) field0 (cycle ds)
    & zipWith (\i (f, _) -> (i, f)) [0 :: Int ..]
    & F.fold do
      h2p <- F.Fold (\m (i, f) -> M.insert (f ^. #height) i m) M.empty id
      p2h <- A.listArray @UArray (0, n0) <$> F.premap (\(_, f) -> f ^. #height) F.list
      ~(h1, h2) <- F.last <&> (\(Just (_, f)) -> fromJust $ findCycle (f ^. #height) (row f))
      pure
        let p1 = h2p M.! h1
            p2 = h2p M.! h2
            (q, r) = (tt - p1) `quotRem` (p2 - p1)
         in (p2h A.! p2 - p2h A.! p1) * q + p2h A.! (p1 + r)
    & pure
  where
    n0 = 10000
    tt = 1000000000000

-- >>> part1
-- 3173

-- >>> part2
-- 1566272189352

data Piece = MkPiece {width :: Int, height :: Int, shape :: Integer} deriving (Generic, Show)
data Coord = C {x :: Int, y :: Int} deriving (Generic, Show)

type instance IxValue Piece = Bool
type instance Index Piece = Coord
instance Ixed Piece where
  type IxKind Piece = A_Lens
  ix (C{x, y}) =
    lens
      (\MkPiece{width, shape} -> testBit shape (x + y * width))
      (\p@MkPiece{width, shape} c -> p{shape = bool clearBit setBit c shape (x + y * width)})

display :: Piece -> String
display p = unlines [[bool '.' '#' $ p ^. ix (C x y) | x <- [0 .. p ^. #width - 1]] | y <- reverse [0 .. p ^. #height - 1]]

row :: Piece -> Int -> Int
row p y = foldl' (\r x -> if p ^. ix (C x y) then setBit r x else x) 0 [0 .. p ^. #width - 1]

plays :: [Piece] -> Piece -> String -> [(Piece, String)]
plays ps field ds = scanl' (\st p -> uncurry (play p) st) (field, ds) ps

play :: Piece -> Piece -> String -> (Piece, String)
play p field ds =
  let (c, ds') = fall (C 2 (field ^. #height + 4)) ds
   in (fromJust $ tryOverlay p c field, ds')
  where
    fall c@(C x y) ds
      | y == 0 || isNothing (tryOverlay p (C x (y - 1)) field) = (c, ds)
      | otherwise = push (C x (y - 1)) ds
    push c@(C x y) ('<' : ds)
      | x > 0 && isJust (tryOverlay p (C (x - 1) y) field) = fall (C (x - 1) y) ds
      | otherwise = fall c ds
    push c@(C x y) ('>' : ds)
      | x + p ^. #width < field ^. #width && isJust (tryOverlay p (C (x + 1) y) field) = fall (C (x + 1) y) ds
      | otherwise = fall c ds

tryOverlay :: Piece -> Coord -> Piece -> Maybe Piece
tryOverlay p (C x0 y0) f@(MkPiece w h sh) =
  ($ MkPiece w (max h $ y0 + p ^. #height) sh)
    <$> foldM
      ((<$>) . (.))
      id
      [ if p ^. ix pc && f ^. ix fc
        then Nothing
        else Just if p ^. ix pc then \f -> f & ix fc .~ True else id
      | px <- [0 .. p ^. #width - 1]
      , py <- [0 .. p ^. #height - 1]
      , let pc = C px py
      , let fc = C (x0 + px) (y0 + py)
      ]

findCycle :: Int -> (Int -> Int) -> Maybe (Int, Int)
findCycle n at = ok $ go 2 (n - 1)
  where
    bb = 256
    mm = 1000000007

    -- Binary search for the cycle length.
    go !l !r
      | r - l <= 1 = l
      | Just _ <- ok m = go m r
      | otherwise = go l m
      where
        m = l + (r - l) `div` 2

    -- Use rolling hash to see if it's possible to have cycles of length `w`.
    -- If it is, return the [begin, end) range of the first cycle.
    ok w =
      zip [0 ..] [w .. n - 1]
        & scanl'
          (\s (i, j) -> (((s - at i * pp) `mod` mm) * bb + at j) `rem` mm)
          (foldl' (\s i -> (s * bb + at i) `rem` mm) 0 [0 .. w - 1])
        & zip [0 ..]
        & flip
          ( foldr
              ( \(i, x) k !m -> case M.lookup x m of
                  Just j | i - j >= w -> Just (i, i + w)
                  _ -> k $ M.insert x i m
              )
              (const Nothing)
          )
          M.empty
      where
        pp = iterate (\x -> (x * bb) `rem` mm) 1 !! (w - 1)
