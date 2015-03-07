module PinkVMBurk where

import WhiteNoise
import Data.Bits
import Data.Word
import Data.Int
lcmrandom :: Word -> Integer
lcmrandom rs = fromIntegral (rs * 196314165 + 907633515)

lcmrandoms:: Word -> [Integer]
lcmrandoms seed = r:lcmrandoms (fromIntegral r)
  where r =  lcmrandom seed

                               
trailingZeros:: Int -> Int
trailingZeros n
   | n == 0        = 0 
   | n  .&. 1 /= 0 = 0
   | otherwise = 1 + trailingZeros (n `shiftR` 1)

pinkRandomBits::Int
pinkRandomBits = 24

pinkRandomShift::Int
pinkRandomShift = (8 * 8)-pinkRandomBits

numOfPinkRows::Int
numOfPinkRows = 30

pinkMax :: Int
pinkMax = (numOfPinkRows + 1) * (1 `shiftL` (pinkRandomBits -1))

pscalarv :: Double
pscalarv = 1.0 / fromIntegral pmax

indexMask::Integer
indexMask = (1 `shiftL` numOfPinkRows) - 1

data PinkNoise = PinkNoise {
                             rows::[Int64]
                           , runningSum::Int64
                           , index::Int
                           , rand2::Int64  
                           } deriving (Show,Eq)

data RandomValue = RandomValue { ridx:: Int
                                 , rval1::Int64
                                 , rval2::Int64
                               } deriving (Show,Eq)

randomValues :: Int -> Int -> [RandomValue]
randomValues rate dur  = map buildVal (zip3 [0..(rate * dur)] (whiteRandom rate dur 100) (whiteRandom rate dur 200))
  where buildVal (idx',val1',val2') = RandomValue idx' (val1' `div` 2) (val2' `div` 2)  
                                       
initialPinkNoise :: PinkNoise
initialPinkNoise = PinkNoise initRows 0 0 0

numOfRows::Int
numOfRows = 30

initRows::[Int64]
initRows = replicate numOfPinkRows 0

updatePink :: PinkNoise-> Int -> Int64->Int64  -> PinkNoise
updatePink pn idx rnd  = PinkNoise
                            updateRow
                            updateRunningSum
                            updateIndex                            
  where updateRunningSum = runningSum pn + rnd - ( rows pn!!idx) 
        updateRow = let sp = splitAt idx (rows pn)
                    in case fst sp of
                        [] -> rnd:snd sp
                        _  -> init (fst sp) ++ [rnd] ++ snd sp
        updateIndex = (index pn + 1) .&. fromIntegral indexMask
        

nextpValue::PinkNoise -> RandomValue -> PinkNoise
nextpValue pn rv | nextIdx /= 0  = updatePink pn
                                   (trailingZeros nextIdx)
                                   (rval1 rv`shiftR` pinkRandomShift)
                                   (rval2 rv`shiftR` pinkRandomShift)
                  | otherwise  = pn {index = nextIdx,rand2 = rval2 rv `shiftR` pinkRandomShift}
  where nextIdx = index pn + 1 .&. fromIntegral indexMask

-- get the pink noise values
getPink::PinkNoise -> Int-> Int -> [PinkNoise]
getPink pn rate dur  = scanl nextpValue pn (randomValues rate dur)

-- generate list of pink floating values
genPink :: PinkNoise -> Int -> Int -> [Double]
genPink pn rate dur = map  calcVal  (getPink pn rate dur)
  where calcVal pn'= scalarv * (fromIntegral (runningSum pn')  + fromIntegral (rand2 pn'))
