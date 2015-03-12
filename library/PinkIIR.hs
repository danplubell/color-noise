module PinkIIR where
import System.Random (randomRs,mkStdGen)

data PinkNoise = PinkNoise {
                              multiplier::[Double]
                            , values::[Double]
                           } deriving (Show,Eq)

type Rate = Int
type Duration = Int
type Poles = Int
type Alpha = Int


initPinkNoise :: Int -> PinkNoise
initPinkNoise p = PinkNoise initMultipliers  initValues
  where initMultipliers = scanl calcMultiplier 1.0  [0.0..fromIntegral p]
        calcMultiplier a i = (i - a/2) * a / (i+1)
        initValues  = take p $ randomRs (0.0,1.0) (mkStdGen 256)

initMultiples:: Int -> Multiples
initMultiples p =  tail $ scanl calcMultiplier  1.0  [0.0..fromIntegral $ p-1]
  where calcMultiplier a i = (i - 1/2) * a / (i + 1)

initialValues :: Int -> Values
initialValues p = take p (map (* 0.01)(randomRs (0.0,1.0) (mkStdGen 256)))

genPinkIIR :: Rate -> Duration -> Samples
genPinkIIR r d = gp (initMultiples 5) (initialValues 5) $ take ( r * d)
                    $ map (*0.1) $ randomRs (0.0,1.0) (mkStdGen 1024)

type Multiples = [Double]
type Values  = [Double]
type Randoms = [Double]
type Samples = [Double]

gp:: Multiples->Values-> Randoms -> Samples
gp _ _ [] = []
gp m v (r:rs) = let nv = calcVal m v r
                in nv : gp m (updateValues v nv) rs
  

calcVal::[Double] -> [Double]-> Double->Double
calcVal a b x =  foldl (-) x (zipWith (*) a b)

updateValues ::[Double]->Double -> [Double]
updateValues xs v = v:tail xs

