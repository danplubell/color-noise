module Main (main) where

import qualified GennoiseBench
-- HASKELETON: import qualified New.ModuleBench

import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain
    [ bgroup "Gennoise" GennoiseBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
