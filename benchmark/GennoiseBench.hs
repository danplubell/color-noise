module GennoiseBench (benchmarks) where

import Gennoise

import Criterion

benchmarks :: [Benchmark]
benchmarks =
    [ bench "main" (nfIO main)
    ]
