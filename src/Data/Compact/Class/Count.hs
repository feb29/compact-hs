{-# LANGUAGE DefaultSignatures #-}

module Data.Compact.Class.Count
    ( Capacity(..)
    , Count1(..)
    , Count0(..)
    )
where

import           Data.Monoid                    ( Sum(..) )
import           Data.Bits                      ( Bits, FiniteBits, popCount )
import           Data.Word

class Capacity a where
    capacity :: a -> Int

class Count1 a where
    count1 :: a -> Int

    default count1 :: (Capacity a, Count0 a) => a -> Int
    count1 bv = capacity bv - count0 bv

class Count0 a where
    count0 :: a -> Int

    default count0 :: (Capacity a, Count1 a) => a -> Int
    count0 bv = capacity bv - count1 bv

instance Capacity Word8 where
    capacity = const 8

instance Capacity Word16 where
    capacity = const 16

instance Capacity Word32 where
    capacity = const 32

instance Capacity Word64 where
    capacity = const 64

instance Count1 Word8 where
    count1 = popCount

instance Count1 Word16 where
    count1 = popCount

instance Count1 Word32 where
    count1 = popCount

instance Count1 Word64 where
    count1 = popCount

instance Count0 Word8
instance Count0 Word16
instance Count0 Word32
instance Count0 Word64

instance Count1 w => Count1 [w] where
    count1 = sum . map count1

instance Count0 w => Count0 [w] where
    count0 = sum . map count0
