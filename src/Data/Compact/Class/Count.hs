{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Compact.Class.Count
    ( Sized(..)
    , Count1(..)
    , Count0(..)
    )
where

import qualified Data.Vector.Unboxed           as U
import           Data.Word

import           Data.Monoid                    ( Sum(..) )
import           Data.Bits                      ( FiniteBits
                                                , finiteBitSize
                                                , popCount
                                                )
import           Foreign.Storable               ( sizeOf )


-- The maximum potential size in bits of the container,
-- but the container is not guaranteed to be able to reach that size:
-- It can fail to allocate at any point before that size is reached.
--
-- A valid range of bit for the container is `[0, capacity xs)`.
class Sized a where
    size :: a -> Int

class Count1 a where
    count1 :: a -> Int
    default count1 :: (Sized a, Count0 a) => a -> Int
    count1 bv = size bv - count0 bv

class Count0 a where
    count0 :: a -> Int
    default count0 :: (Sized a, Count1 a) => a -> Int
    count0 bv = size bv - count1 bv

instance Sized Word8  where size = const 8
instance Sized Word16 where size = const 16
instance Sized Word32 where size = const 32
instance Sized Word64 where size = const 64

instance Count1 Word8  where count1 = popCount
instance Count1 Word16 where count1 = popCount
instance Count1 Word32 where count1 = popCount
instance Count1 Word64 where count1 = popCount

instance Count0 Word8
instance Count0 Word16
instance Count0 Word32
instance Count0 Word64

instance Count1 w => Count1 [w] where
    count1 = sum . map count1

instance Count0 w => Count0 [w] where
    count0 = sum . map count0

instance Sized (U.Vector a) where
    size = const (maxBound :: Int)

instance (U.Unbox w, Count1 w) => Count1 (U.Vector w) where
    count1 = U.sum . U.map count1

instance (U.Unbox w, Count1 w) => Count0 (U.Vector w)

