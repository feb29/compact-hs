{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
( BitSized(..)
, BitCount(..)

, Uint(..)
, Raw(..)
) where

import qualified Data.Bits as B
import           Data.Monoid (Sum(..))
import           Data.Word (Word64)

class BitSized a where
  size :: Integral u => a -> u
  -- return the number of non-zero bits; a.k.a hamming weight or population count.
  ones :: Integral u => a -> u

class BitCount a where
  count1 :: Integral u => a -> u

  default count1 :: (BitSized a, Integral u) => a -> u
  count1 = ones

  count0 :: Integral u => a -> u

  default count0 :: (BitSized a, Integral u) => a -> u
  count0 t = size t - ones t

class BitCount a => BitRank a where
  rank1 :: Integral u => a -> u -> u
  rank1 t i = i - rank0 t i

  rank0 :: Integral u => a -> u -> u
  rank0 t i = i - rank1 t i

class BitSelect1 a where
  select1 :: Integral u => a -> u -> Maybe u

  default select1 :: (BitRank a, Integral u) => a -> u -> Maybe u
  select1 t i = if i >= count1 t then Nothing else Just selected
    where
      capacity = undefined
      selected = search capacity (\j -> rank1 t i >= i)

class BitSelect0 a where
  select0 :: Integral u => a -> u -> Maybe u

  default select0 :: (BitRank a, Integral u) => a -> u -> Maybe u
  select0 t i = if i >= count0 t then Nothing else Just selected
    where
      capacity = undefined
      selected = search capacity (\j -> rank0 t i >= i)

newtype Uint f = Uint { uint :: f }
  deriving
    ( Show
    , Eq
    , Ord
    , Enum
    , Bounded
    , Num
    , Real
    , Integral
    , B.Bits
    , B.FiniteBits
    )

data Raw w = Raw [w]
  deriving
    ( Show
    , Eq
    , Ord
    )

instance B.FiniteBits w => BitSized (Uint w) where
  size = fromIntegral . B.finiteBitSize
  ones = fromIntegral . B.popCount

instance B.FiniteBits w => BitCount (Uint w)

instance BitCount w => BitSized (Raw w) where
  size = const 65536
  ones (Raw ws) = count1 ws

instance BitCount w => BitCount (Raw w)

instance BitCount w => BitCount [w] where
  count1 = sum . (map count1)
  count0 = sum . (map count0)

search :: (Integral u, Ord u) => u -> (u -> Bool) -> u
search end fun = search' 0 end
  where
    search' !i !j
      | i <  j = i
      | i >= j = if p then search' i h else search' (h + 1) j
        where
          h = i + (j - i) `div` 2
          p = fun h
