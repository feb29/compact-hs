{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Compact.Class.Select
    ( Select0(..)
    , Select1(..)
    )
where

import           Data.Compact.Class.Count       ( Capacity(..)
                                                , Count0(..)
                                                , Count1(..)
                                                )
import           Data.Compact.Class.Rank        ( Rank0(..)
                                                , Rank1(..)
                                                )
class Select0 a where
    select0 :: a -> Int -> Maybe Int

    default select0 :: (Capacity a, Rank0 a) => a -> Int -> Maybe Int
    select0 bv c
        | c >= count0 bv = Nothing
        | otherwise      = Just selected where
        selected = search (capacity bv) (\j -> rank0 bv j >= c)


class Select1 a where
    select1 :: a -> Int -> Maybe Int

    default select1 :: (Capacity a, Rank1 a) => a -> Int -> Maybe Int
    select1 bv c
        | c >= count1 bv = Nothing
        | otherwise      = Just selected where
        selected = search (capacity bv) (\j -> rank1 bv j >= c)


search :: (Integral u, Ord u) => u -> (u -> Bool) -> u
search end fun = search' 0 end
  where
    search' !i !j | i < j  = i
                  | i >= j = if p then search' i h else search' (h + 1) j
      where
        h = i + (j - i) `div` 2
        p = fun h

