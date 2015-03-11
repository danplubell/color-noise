module WhiteNoise (whiteSamples,whiteRandom) where
import System.Random
{-
Generate white noise with values between -0.1 and 0.1
-}

whiteRandom :: Int -> [Double]
whiteRandom s = randomRs (-0.1, 0.1) (mkStdGen s)

whiteSamples :: Int->Int->Int -> [Double]
whiteSamples r d s = take (r * d) (whiteRandom s)

