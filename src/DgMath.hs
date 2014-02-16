module DgMath
    (
      evalMonteCarlo
    , meanStdErr
    , nUniform
    , nGaussian
    , prettyPrint
    , OptionParams (VanillaOption, stockPrice, strike, expiry, payoffFn)
    , MCSimulation (MCSimulation, numPaths, numSteps, seed, stochasticProcess)
    , PayoffFn (EuroPut, EuroCall, AsianPut, AsianCall)
    , StochasticProcess (Gaussian, GaussianAntithetic)
    ) where

import Control.Monad.State (State, evalState, liftM, replicateM, runState, state)
import Data.List
import Data.Int
import Text.Printf

type DgRandom a = State Int32 a

data MCSimulation =
  MCSimulation {   numPaths          :: Int
                 , numSteps          :: Int
                 , seed              :: Int32
                 , stochasticProcess :: StochasticProcess
               } deriving (Show, Read)

data OptionParams =
  VanillaOption {   stockPrice  :: Double
                  , strike      :: Double
                  , expiry      :: Double
                  , payoffFn    :: PayoffFn
                } deriving (Show, Read)

data PayoffFn = EuroPut | EuroCall | AsianPut | AsianCall deriving (Show, Read)

data StochasticProcess =  Gaussian | GaussianAntithetic deriving (Show, Read)

class PrettyPrint a
  where
    prettyPrint :: a -> String

instance PrettyPrint Double
  where
    prettyPrint d = printf "%1.4f " d

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b)
  where
    prettyPrint (a, b) = "(" ++ (prettyPrint a) ++ ", " ++ (prettyPrint b) ++ ")"

m = 2147483647  -- Ref: "Random Number Generators: Good Ones Are Hard To Find", Park & Miller
a = 16807       -- Ref: "Random Number Generators: Good Ones Are Hard To Find", Park & Miller

dgRange :: (Int32,Int32)
dgRange = (0, m-1)

-- Minimal Standard Random Number Generator
-- Ref: "Random Number Generators: Good Ones Are Hard To Find", Park & Miller
msrngSt :: DgRandom Double
msrngSt = state msrng 

msrng :: Int32 -> (Double, Int32)
msrng seed = (newRand, newSeed)
  where
    (q, r) = m `divMod` a
    (c, d) = seed `divMod` q
    t  = a*d - r*c 
    newRand = fromIntegral newSeed / fromIntegral m
    newSeed = if t > 0 then t else t+m

-- Beasley-Springer-Moro algorithm for approximating the inverse normal
-- Ref: "Monte Carlo Methods in Financial Engineering", Glasserman
bsmInvNormal :: Double -> Double
bsmInvNormal u 
   | abs y < 0.42 = x1 y (y*y)
   | y < 0 =  negate . x2 . log $ -log r
   | otherwise = x2 . log $ -log r
  where
    y = u - 0.5
    r = if y > 0 then 1 - u else u
    x1 z r = z*(((a3*r + a2)*r + a1)*r + a0)/((((b3*r + b2)*r + b1)*r + b0)*r + 1)
    x2 r = c0 + r*(c1 + r*(c2 + r*(c3 + r*(c4+ r*(c5 + r*(c6 + r*(c7 + r*c8)))))))
    a0 =   2.50662823884
    a1 = -18.61500062529
    a2 =  41.39119773534
    a3 = -25.44106049637
    b0 =  -8.47351093090
    b1 =  23.08336743743
    b2 = -21.06224101826
    b3 =   3.13082909833
    c0 = 0.3374754822726147
    c1 = 0.9761690190917186
    c2 = 0.1607979714918209
    c3 = 0.0276438810333863
    c4 = 0.0038405729373609
    c5 = 0.0003951896511919
    c6 = 0.0000321767881768
    c7 = 0.0000002888167364
    c8 = 0.0000003960315187

oneUniform :: DgRandom Double
oneUniform = msrngSt

nUniform :: Int -> Int32 -> [Double]
-- Generate a Uniform distribution between 0 & 1
nUniform n = evalState $ replicateM n oneUniform
  where oneUniform = msrngSt

oneGaussian :: DgRandom Double
oneGaussian = liftM bsmInvNormal msrngSt

nGaussian :: Int -> Int32 -> [Double]
-- Generate a list of n samples from a Normal (Gaussian) distribution with mean 0 & standard deviation 1
nGaussian n = evalState $ replicateM n oneGaussian

mnGaussian :: Int -> Int -> Int32 -> [[Double]]
-- Generate m lists of n samples from a Normal (Gaussian) distribution with mean 0 & standard deviation 1
mnGaussian m n seed
   | m < 1 = []
   | m == 1 = x : []
   | otherwise = x : xs
  where
    (x, newSeed) = (runState $ replicateM n oneGaussian) seed
    xs = mnGaussian (m-1) n newSeed

mnGaussianAntithetic :: Int -> Int -> Int32 -> [[Double]]
mnGaussianAntithetic m n seed 
   | m < 1 = []
   | m `mod` 2 == 1 = []
   | m == 2 =  x : [x']
   | otherwise = x : x' : xs 
  where
    (x, newSeed) = (runState $ replicateM n oneGaussian) seed
    x' = map negate x
    xs = mnGaussianAntithetic (m-2) n newSeed

nextStockPrice :: Double -> Double -> Double -> Double -> Double -> Double
nextStockPrice rate vol dT oldS dZ = oldS * exp (a + b*dZ)
  where
    a = (rate - vol^2 * 0.5)*dT
    b = vol * sqrt dT

pv :: Double -> Double -> Double -> Double
pv rate t s = s * exp ((-rate) * t)

pv' :: Double -> Double -> (Double, Double) -> (Double, Double)
pv' rate t (s, se) = (s * exp (-rate * t), se)

genPricePath :: Double -> Double -> Double -> (Double -> [Double] -> [Double])
-- generate Price Path
-- note: s0 is included as the first element of list
genPricePath rate vol dT = scanl (nextStockPrice rate vol dT)

genPricePaths :: Double -> Double -> Double -> (Double -> [[Double]] -> [[Double]])
-- generate Price Paths m x n
genPricePaths rate vol dT s0 = map (genPricePath rate vol dT s0)

evalMonteCarlo :: OptionParams -> Double -> Double -> MCSimulation -> (Double, Double)
evalMonteCarlo optionParams rate vol mcSim = pv' rate t . meanStdErr . poFn . genPricePaths rate vol dT s0 $ stochProc numPaths' numSteps' seed'
  where
    s0 = stockPrice optionParams
    k = strike optionParams
    t = expiry optionParams
    poFn = case payoffFn optionParams of
      EuroPut -> euroPut k
      EuroCall -> euroCall k
      AsianPut -> asianPut k
      AsianCall -> asianCall k
    dT = t / fromIntegral numSteps'
    numPaths' = numPaths mcSim
    numSteps' = numSteps mcSim
    seed' = seed mcSim
    stochProc = case stochasticProcess mcSim of
      Gaussian -> mnGaussian
      GaussianAntithetic -> mnGaussianAntithetic

mapEvalFn :: (Double -> Double) -> ([Double] -> Double) -> ([[Double]] -> [Double])
mapEvalFn payoffFn selectFn = map $ payoffFn . selectFn

callPayoffFn :: Double -> Double -> Double
callPayoffFn k sn = max (sn-k) 0

putPayoffFn :: Double -> Double -> Double
putPayoffFn k sn = max (k-sn) 0

asianCall :: Double -> [[Double]] -> [Double]
asianCall k = mapEvalFn (callPayoffFn k) (mean . tail)

asianPut :: Double -> [[Double]] -> [Double]
asianPut k = mapEvalFn (putPayoffFn k) (mean . tail) 

euroCall :: Double -> [[Double]] -> [Double]
euroCall k = mapEvalFn (callPayoffFn k) last

euroPut :: Double -> [[Double]] -> [Double]
euroPut k = mapEvalFn (putPayoffFn k) last 

sqr :: Num a => a -> a
sqr a = a * a

se xs m = (sd xs m)/n_1
  where 
    n_1 = (fromIntegral $ length xs) - 1

mean :: [Double] -> Double
mean xs = acc / fromIntegral n
  where
    (n, acc) = foldl' k (0,0) xs
    k (n, acc) x = n `seq` acc `seq` (n+1, acc+x)

sd :: [Double] -> Double -> Double
sd xs m = sqrt $ (foldl' (\acc x -> acc + (x - m)^2) 0 xs / n)
  where 
   n  = fromIntegral $ length xs

-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
meanStdErr :: [Double] -> (Double, Double)
meanStdErr xs = (mean'', stdErr)
  where
    (n'', mean'', m2'') = foldl' k (0, 0, 0) xs
    variance = m2'' / (n''-1) -- unbiased sample variance using Bessel's correct (n-1) instead of n
    sampStdDev = sqrt variance
    stdErr = sampStdDev / sqrt n''
    k (n', mean', m2') x = n `seq` mean `seq` m2 `seq` (n, mean, m2)
      where
        n = n' + 1
        delta = x - mean'
        mean = mean' + delta/n
        m2 = m2' + delta * (x - mean)



