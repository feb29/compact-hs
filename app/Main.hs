module Main where

import qualified Data.Vector.Unboxed           as U
import           Data.Word                      ( Word64 )
import           Data.Compact.Class             ( Count1(..) )

main :: IO ()
main = print $ count1 (U.fromList [1 :: Word64, 2, 3])

