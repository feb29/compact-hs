module Main where

import           Data.Word                      ( Word64 )
import           Data.Compact.Class.Count       ( Count1(..) )

main :: IO ()
main = print (count1 [1 :: Word64, 2, 3])

