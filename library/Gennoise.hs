module Gennoise (module Gennoise) where

import System.IO
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.List (find)
import Data.Maybe (isJust,fromJust)
import Sound.Wav
import Sound.Wav.ChannelData
import System.Random
import Data.Int
import qualified Data.Vector as V 
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Bits


data Color = White | Brown | Pink deriving (Eq,Show)

data Flag = Version | Help | Duration Int | SampleRate Int | NoiseColor Color |NoValue deriving (Eq,Show)

data GenInfo = GenInfo {   sampleRate::Int
                         , noiseColor:: Color
                         , duration::Int
                       } deriving (Show,Eq)
                 
options :: [OptDescr Flag]
options = [
            Option "h" ["help"] (NoArg Help) "show this help message"
          , Option "d" ["duration"] (OptArg parseDuration "Duration")
            "the duration that the noise will play"
          , Option "r" ["samplerate"] (OptArg parseRate "Rate") "the sample rate"
          , Option "c" ["color"] (OptArg parseColor "Color") "the noise color, white, brown, pink"
            
          ]
          
parseDuration :: Maybe String -> Flag
parseDuration (Just s) = Duration (read s)
parseDuration Nothing  = NoValue

parseRate :: Maybe String -> Flag
parseRate (Just s) = SampleRate (read s) 
parseRate Nothing = NoValue
  
parseColor :: Maybe String -> Flag
parseColor (Just s) = case s of
                 "white" -> NoiseColor White
                 "brown" -> NoiseColor Brown
                 "pink"  -> NoiseColor Pink
                 _       -> NoiseColor White
parseColor Nothing = NoValue

main :: IO ()
main = do
  args <- getArgs
--  print $ getOpt Permute options args

  case getOpt Permute options args of
    (flags,[filename],[]) | not (hasColor flags) -> do
                                    putStrLn  "No color parameter given"
                                    putStrLn  usageMsg
                          | not (hasDuration flags) -> do
                                    putStrLn  "No duration parameter given"
                                    putStrLn usageMsg
                          | not (hasSampleRate flags) -> do
                                    putStrLn "No sample rate parameter given"
                                    putStrLn usageMsg
                          | otherwise ->  handleFlags flags filename
    (_,_,msgs@(_:_)) -> error $ concat msgs ++ usageMsg                                          
    (_,filenames@(_:_),_) -> putStrLn $  "Invalid parameters: " ++ show filenames 
    (_, [], _) -> do putStrLn "no output filename provided; please provide one"
                     putStrLn usageMsg


hasColor::[Flag] -> Bool
hasColor f = isJust $ find isColor f

isColor::Flag -> Bool
isColor (NoiseColor _ ) = True
isColor _               = False

fromColor::Flag -> Color
fromColor (NoiseColor c) = c
fromColor _              = error  "not a color parameter"

hasDuration::[Flag] -> Bool
hasDuration f = isJust $ find isDuration f

isDuration::Flag -> Bool
isDuration (Duration _ ) = True
isDuration _              = False

fromDuration :: Flag  -> Int
fromDuration (Duration a) = a
fromDuration _            = error "not a duration value"

hasSampleRate:: [Flag] -> Bool
hasSampleRate f = isJust $ find isSampleRate f

isSampleRate::Flag -> Bool
isSampleRate (SampleRate _) = True
isSampleRate _              = False                             

fromSampleRate::Flag -> Int
fromSampleRate (SampleRate a) = a
fromSampleRate _              = error "not a sample rate"

header ::String
header = "Usage: gennoise <filename> [OPTIONS]"

usageMsg::String
usageMsg = usageInfo header options

handleFlags:: [Flag] -> FilePath -> IO()
handleFlags flags filepath
  | Help `elem` flags = putStrLn usageMsg
  | otherwise = do
                  putStrLn $ "Generating noise in file named: " ++ filepath
                  let genInfo = formatGenInfo flags
                  let d = case noiseColor genInfo of
                             White -> generateWhiteNoise (sampleRate genInfo) (duration genInfo)
                             Brown -> generateBrownNoise (sampleRate genInfo) (duration genInfo)
                             Pink  -> generatePinkNoise (sampleRate genInfo) (duration genInfo)
                  encodeWaveFile filepath d
                  putStrLn "[done]"

formatGenInfo :: [Flag] -> GenInfo
formatGenInfo flags = GenInfo { duration = fromDuration $ fromJust $ find isDuration flags
                              , sampleRate = fromSampleRate $ fromJust $ find isSampleRate flags
                              , noiseColor = fromColor $ fromJust $ find isColor flags
                              }             
format:: Int->WaveFormat
format rate = WaveFormat
    { waveAudioFormat = MicrosoftPCM
    , waveNumChannels = 1
    , waveSampleRate = fromIntegral rate
    , waveByteRate = fromIntegral rate  * 2
    , waveBlockAlignment = 2
    , waveBitsPerSample = 16
    }


waveFileTemplate::Int -> WaveFile
waveFileTemplate rate  = WaveFile { waveFormat = format rate 
                             , waveData = BL.empty
                             , waveFact = Nothing
                             , waveInfo = Nothing
                             }

whiteRandom :: Int -> Int -> Int -> [Int64]
whiteRandom rate dur seed =  take (rate * dur) (map (`div` 2)(randomRs (minBound,maxBound) (mkStdGen seed)))

whiteSamples :: Int -> Int -> Int -> [Double]
whiteSamples rate dur seed  = map calcVal (whiteRandom rate dur seed) 
  where
    calcVal v = scalarv * fromIntegral (v `shiftR` pinkRandomShift)
    
generateWhiteNoise::Int->Int -> WaveFile
generateWhiteNoise rate dur =encodeFloatingWaveData (waveFileTemplate rate ) (FloatingWaveData [V.fromList  (whiteSamples rate dur 100) ])

generateBrownNoise::Int->Int -> WaveFile
generateBrownNoise _ _ = undefined

generatePinkNoise::Int->Int -> WaveFile 
generatePinkNoise rate dur  = encodeFloatingWaveData (waveFileTemplate rate) (FloatingWaveData [V.fromList (genPink initialPinkNoise rate dur)])

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

pmax :: Int
pmax = (numOfRows + 1) * (1 `shiftL` (pinkRandomBits -1))

scalarv :: Double
scalarv = 1.0 / fromIntegral pmax

indexMask::Integer
indexMask = (1 `shiftL` numOfRows) - 1

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
initRows = replicate numOfRows 0

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
        


--build pink wave from file of doubles
buildPink :: IO()
buildPink = do
  handle <- openFile "pinkvalues.txt" ReadMode
  contents <- hGetContents handle
  let e = encodeFloatingWaveData (waveFileTemplate 8000) (FloatingWaveData [V.fromList ( map rdIt (lines contents))])
  encodeWaveFile "testpink.wav" e
  return ()
rdIt :: String -> Double
rdIt  str = read str::Double
