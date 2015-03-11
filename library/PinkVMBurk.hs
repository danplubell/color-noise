{-Generation of pink noise roughly based on the algorithm from here:
http://www.firstpr.com.au/dsp/pink-noise/phil_burk_19990905_patest_pink.c

From here:
http://www.firstpr.com.au/dsp/pink-noise/

-}
module PinkVMBurk (genPink,initialPinkNoise) where

import WhiteNoise (whiteRandom)
import Data.Bits
trailingZeros:: Int -> Int
trailingZeros n
   | n == 0        = 0 
   | n  .&. 1 /= 0 = 0
   | otherwise = 1 + trailingZeros (n `shiftR` 1)

numOfPinkRows::Int
numOfPinkRows = 30

indexMask::Integer
indexMask = (1 `shiftL` numOfPinkRows) - 1

initialPinkNoise :: PinkNoise
initialPinkNoise = PinkNoise initRows 0 0 0


initRows::[Double]
initRows = replicate numOfPinkRows 0.0

data PinkNoise = PinkNoise {
                             rows::[Double]
                           , runningSum::Double
                           , index::Int
                           , rand2::Double  
                           } deriving (Show,Eq)

data RandomValue = RandomValue {   rval1::Double
                                 , rval2::Double
                               } deriving (Show,Eq)
        
randomValues :: Int -> Int -> [RandomValue]
randomValues rate dur  = map buildVal (zip3 [0..(rate * dur)] (whiteRandom 100) (whiteRandom 200))
  where buildVal ( _ ,val1',val2') = RandomValue  (val1' * 0.1)  (val2' * 0.1)  

getPink::PinkNoise -> Int-> Int -> [PinkNoise]
getPink pn rate dur  = scanl nextpValue pn (randomValues rate dur)

-- generate list of pink floating values
genPink :: PinkNoise -> Int -> Int -> [Double]
genPink pn rate dur = map  calcVal  (getPink pn rate dur)
  where calcVal pn'= runningSum pn'  + rand2 pn'

updatePink :: PinkNoise-> Int -> Double->Double  -> PinkNoise
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
                                   (rval1 rv)
                                   (rval2 rv)
                  | otherwise  = pn {index = nextIdx,rand2 = rval2 rv}
  where nextIdx = index pn + 1 .&. fromIntegral indexMask
