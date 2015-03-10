module WhiteNoise where
import Data.Int
import System.Random
import Data.Bits

randomBits::Int
randomBits = 24

randomShift::Int
randomShift = (8 * 8)-randomBits

numOfRows::Int
numOfRows = 30

pmax :: Int
pmax = (numOfRows + 1) * (1 `shiftL` (randomBits -1))

scalarv :: Double
scalarv = 1.0 / fromIntegral pmax

whiteRandom :: Int -> Int -> Int -> [Int64]
whiteRandom rate dur seed =  take (rate * dur) (map (`div` 2)(randomRs (minBound,maxBound) (mkStdGen seed)))

whiteSamples :: Int -> Int -> Int -> [Double]
whiteSamples rate dur seed  = map calcVal (whiteRandom rate dur seed) 
  where
    calcVal v = scalarv * fromIntegral (v `shiftR` randomShift)

