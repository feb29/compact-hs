module Main where

import           Criterion.Main
import           Data.Compact.Class
import           Data.Word
import qualified Data.Vector.Unboxed           as U

main = defaultMain
    [ bgroup
        "Count1"
        [ bgroup
            "[Word64]"
            [ bench "1000" $ whnf count1 (replicate 1000 (17 :: Word64))
            , bench "2000" $ whnf count1 (replicate 2000 (17 :: Word64))
            , bench "3000" $ whnf count1 (replicate 3000 (17 :: Word64))
            ]
        , bgroup
            "U.Vector Word64"
            [ bench "1000" $ whnf count1 (U.replicate 1000 (17 :: Word64))
            , bench "2000" $ whnf count1 (U.replicate 2000 (17 :: Word64))
            , bench "3000" $ whnf count1 (U.replicate 3000 (17 :: Word64))
            ]
        ]

    , bgroup
        "Rank1"
        [ bgroup
            "Word32"
            [ bench "10" $ whnf rank1 (10 :: Word32)
            , bench "20" $ whnf rank1 (20 :: Word32)
            , bench "30" $ whnf rank1 (30 :: Word32)
            ]
        , bgroup
            "Word64"
            [ bench "10" $ whnf rank1 (17 :: Word64)
            , bench "20" $ whnf rank1 (17 :: Word64)
            , bench "30" $ whnf rank1 (17 :: Word64)
            ]
        ]
    ]

