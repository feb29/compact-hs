module Main where

import Lib
import Data.Word (Word64)

main :: IO ()
main = do
  print (sizeofWord :: Int)
  print (sizeofRaw  :: Integer)
  print (count1 [one, 2, 3])
  print (count0 [one, 2, 3])
    where
      one :: Uint Word64
      one = 1

      sizeofWord = size (undefined :: Uint Word64)
      sizeofRaw  = size (undefined :: Raw  (Uint Word64))
