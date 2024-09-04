module Sample (samplePlanets) where

import System.Random (getStdGen, randomRs)
import Data.Maybe (catMaybes)
import Normal (normalSample)

rejectionSampleNormal :: (Int, Double) -> Maybe Int 
rejectionSampleNormal (x, y) = if y < yTest then Just x else Nothing 
    where yTest = normalSample x

chunk3 :: [a] -> [(a,a,a)]
chunk3 (x:y:z:xs) = (x, y, z) : chunk3 xs 
chunk3 _ = []

samplePlanets :: Int -> IO [(Int, Int, Int)]
samplePlanets n = do 
    g <- getStdGen 
    let xs = randomRs (1, 125) g 
    let ys = (/ 4000.0) <$> fromIntegral <$> randomRs (1 :: Int, 4000) g
    let samples = catMaybes $ rejectionSampleNormal <$> zip xs ys 

    pure $ take n (chunk3 samples)
