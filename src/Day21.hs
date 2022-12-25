{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day21 where

import qualified Data.ByteString.Char8 as B
import Data.List (foldl')
import qualified Data.Map.Lazy as M

rootExpr :: forall e. Expr e => IO e
rootExpr = do
  lns <- B.lines <$> B.readFile "src/Day21.txt"
  let
    resolve x = case B.readInt x of
      Just (n, _) -> lit n
      Nothing -> (if x == "humn" then hmn else id) $ mm M.! x
    mm =
      foldl'
        ( \m ln -> case B.words ln of
            [B.init -> a, B.readInt -> Just (b, _)] -> M.insert a (lit b) m
            [B.init -> a, b, B.head -> op, c] ->
              M.insert
                a
                ( (if a == "root" then root else bin)
                    (parseOp op)
                    (resolve b)
                    (resolve c)
                )
                m
        )
        M.empty
        lns
  pure $ mm M.! "root"

part1 = rootExpr @Int
part2 = (\(Done x) -> x) <$> rootExpr @Solve

-- >>> part1
-- 168502451381566

-- >>> part2
-- 3343167719435

data Op = Add | Sub | Mul | Div

parseOp = \case
  '+' -> Add
  '-' -> Sub
  '*' -> Mul
  '/' -> Div

class Expr e where
  lit :: Int -> e
  hmn :: e -> e
  root :: Op -> e -> e -> e
  bin :: Op -> e -> e -> e

instance Expr Int where
  lit = id
  hmn = id
  root = bin
  bin Add = (+)
  bin Sub = (-)
  bin Mul = (*)
  bin Div = quot

data Solve = Done Int | Unkn (Int -> Int)

instance Expr Solve where
  lit = Done
  hmn _ = Unkn id

  root _ (Done x) (Unkn f) = Done $ f x
  root _ (Unkn f) (Done x) = Done $ f x

  bin op (Done x) (Done y) = Done $ bin op x y
  bin Add (Done x) (Unkn f) = Unkn \r -> f (r - x)
  bin Add (Unkn f) (Done x) = Unkn \r -> f (r - x)
  bin Sub (Done x) (Unkn f) = Unkn \r -> f (x - r)
  bin Sub (Unkn f) (Done x) = Unkn \r -> f (x + r)
  bin Mul (Unkn f) (Done x) = Unkn \r -> f (r `quot` x)
  bin Mul (Done x) (Unkn f) = Unkn \r -> f (r `quot` x)
  bin Div (Unkn f) (Done x) = Unkn \r -> f (r * x)
  bin Div (Done x) (Unkn f) = Unkn \r -> f (x `quot` r)

-- data Expr = Unkn | Lit Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr deriving (Show)

-- simplify x@Unkn = x
-- simplify x@(Lit _) = x
-- simplify (Add x y) = case (simplify x, simplify y) of
--   (Lit x, Lit y) -> Lit $ x + y
--   (x, y) -> Add x y
-- simplify (Sub x y) = case (simplify x, simplify y) of
--   (Lit x, Lit y) -> Lit $ x - y
--   (x, y) -> Sub x y
-- simplify (Mul x y) = case (simplify x, simplify y) of
--   (Lit x, Lit y) -> Lit $ x * y
--   (x, y) -> Mul x y
-- simplify (Div x y) = case (simplify x, simplify y) of
--   (Lit x, Lit y) -> Lit $ x `quot` y
--   (x, y) -> Div x y

-- solve Unkn (Lit x) = x
-- solve (Add (Lit x) u) (Lit y) = solve u $ Lit $ y - x
-- solve (Add u (Lit x)) (Lit y) = solve u $ Lit $ y - x
-- solve (Sub (Lit x) u) (Lit y) = solve u $ Lit $ x - y
-- solve (Sub u (Lit x)) (Lit y) = solve u $ Lit $ x + y
-- solve (Mul (Lit x) u) (Lit y) = solve u $ Lit $ y `quot` x
-- solve (Mul u (Lit x)) (Lit y) = solve u $ Lit $ y `quot` x
-- solve (Div (Lit x) u) (Lit y) = solve u $ Lit $ x `quot` y
-- solve (Div u (Lit x)) (Lit y) = solve u $ Lit $ x * y

-- solveRoot (Add x y) = solve x y
-- solveRoot (Sub x y) = solve x y
-- solveRoot (Mul x y) = solve x y
-- solveRoot (Div x y) = solve x y
