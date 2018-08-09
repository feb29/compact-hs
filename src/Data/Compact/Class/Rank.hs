{-# LANGUAGE DefaultSignatures #-}

module Data.Compact.Class.Rank
    ( Rank0(..)
    , Rank1(..)
    )
where

import           Data.Compact.Class.Count       ( Count0(..)
                                                , Count1(..)
                                                )
class Count0 a => Rank0 a where
    rank0 :: a -> Int -> Int

    default rank0 :: Rank1 a => a -> Int -> Int
    rank0 bv i = i - rank1 bv i


class Count1 a => Rank1 a where
    rank1 :: a -> Int -> Int

    default rank1 :: Rank0 a => a -> Int -> Int
    rank1 bv i = i - rank0 bv i

