{-# LANGUAGE DefaultSignatures #-}

module Data.Compact.Class.Rank
    ( Rank0(..)
    , Rank1(..)
    )
where

import           Data.Word
import           Data.Bits                      ( FiniteBits
                                                , (.&.)
                                                , bit
                                                , complement
                                                )
import           Data.Compact.Bit               ( address )
import           Data.Compact.Class.Count       ( Sized(..)
                                                , Count0(..)
                                                , Count1(..)
                                                )


class Count1 a => Rank1 a where
    rank1 :: a -> Int -> Int
    default rank1 :: Rank0 a => a -> Int -> Int
    rank1 bv i = i - rank0 bv i


class Count0 a => Rank0 a where
    rank0 :: a -> Int -> Int
    default rank0 :: Rank1 a => a -> Int -> Int
    rank0 bv i = i - rank1 bv i


instance Rank1 Word8  where rank1 = wordRank1
instance Rank1 Word16 where rank1 = wordRank1
instance Rank1 Word32 where rank1 = wordRank1
instance Rank1 Word64 where rank1 = wordRank1

instance Rank0 Word8  where rank0 = wordRank0
instance Rank0 Word16 where rank0 = wordRank0
instance Rank0 Word32 where rank0 = wordRank0
instance Rank0 Word64 where rank0 = wordRank0


wordRank1 :: (FiniteBits w, Num w, Sized w, Count1 w) => w -> Int -> Int
wordRank1 w 0 = 0
wordRank1 w i | i < 0       = undefined
              | i >= size w = count1 w
              | otherwise   = count1 $ w .&. (bit i - 1)

wordRank0 :: (FiniteBits w, Num w, Sized w, Count1 w) => w -> Int -> Int
wordRank0 w = wordRank1 (complement w)


instance (Sized w, Rank1 w) => Rank1 [w] where
    rank1 ws i = count + rank
      where
        (q, r) = address i (size $ head ws) -- FIXME
        count  = count1 $ take q ws
        rank   = rank1 (ws !! q) r

instance (Sized w, Count1 w, Count0 w, Rank1 w) => Rank0 [w]
