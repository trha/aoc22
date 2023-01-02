{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Day25 where
import qualified Data.ByteString.Char8 as B
import Data.Char (intToDigit)
import Data.Functor ((<&>))
import Control.Arrow ((>>>))

part1 = B.readFile "src/Day25.txt" <&> (B.lines >>> map fromSnafu >>> sum >>> toSnafu)

-- >>> part1
-- "2=--=0000-1-0-=1=0=2"

fromSnafu = B.foldl' (\x i -> x * 5 + val i) 0
  where
    val = \case
      '2' -> 2
      '1' -> 1
      '0' -> 0
      '-' -> -1
      '=' -> -2

toSnafu = B.reverse . B.unfoldr \case 
  0 -> Nothing
  q | (q', r) <- q `quotRem` 5 -> Just case r of
    -- Carry 1 as 3 * 5^n = (5 - 2) * 5^n = 1 * 5^(n + 1) - 2 * 5^n.
    3 -> ('=', q' + 1)
    4 -> ('-', q' + 1)
    _ -> (intToDigit r, q')
