{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | TODO
module Gennoise (module Gennoise) where

import System.Environment (getArgs)
import System.Console.GetOpt
import Data.List (find)
import Data.Maybe (isNothing,isJust,fromJust)
import Sound.Wav
import Sound.Wav.ChannelData

-- HASKELETON: import New.Module as Gennoise

-- | TODO

data Color = White | Brown | Pink deriving (Eq,Show)

data Flag = Version | Help | Duration Integer | SampleRate Integer | NoiseColor Color |NoValue deriving (Eq,Show)

data GenInfo = GenInfo {   sampleRate::Integer
                         , noiseColor:: Color
                         , duration::Integer
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

fromDuration :: Flag  -> Integer
fromDuration (Duration a) = a
fromDuration _            = error "not a duration value"

hasSampleRate:: [Flag] -> Bool
hasSampleRate f = isJust $ find isSampleRate f

isSampleRate::Flag -> Bool
isSampleRate (SampleRate _) = True
isSampleRate _              = False                             

fromSampleRate::Flag -> Integer
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
                             _                       -> IntegralWaveData []

                  encodeWaveFile filepath (encodeIntegralWaveData waveFileTemplate d)
                  putStrLn "[done]"

formatGenInfo :: [Flag] -> GenInfo
formatGenInfo flags = GenInfo { duration = fromDuration $ fromJust $ find isDuration flags
                              , sampleRate = fromSampleRate $ fromJust $ find isSampleRate flags
                              , noiseColor = fromColor $ fromJust $ find isColor flags
                              }             
--format:: [Flag]->WaveFormat
--format flags = 

waveFile::IntegralWaveData->WaveFile
waveFile = encodeIntegralWaveData waveFileTemplate 

waveFileTemplate::WaveFile
waveFileTemplate = undefined

integralWaveData::IntegralWaveData
integralWaveData = IntegralWaveData []

generateWhiteNoise::Integer->Integer -> IntegralWaveData
generateWhiteNoise s r = IntegralWaveData [] 

generateBrownNoise::Integer->Integer -> IntegralWaveData
generateBrownNoise s r = undefined

generatePinkNoise::Integer->Integer -> IntegralWaveData
generatePinkNoise s r = undefined
