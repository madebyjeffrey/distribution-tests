module Main (main) where
import Control.Monad (forM_)

import Sample (samplePlanets)

toShow :: (Int, Int, Int) -> String 
toShow (x, y, z) = show x <> ", " <> show y <> ", " <> show z 

main :: IO ()
main = do
  planets <- samplePlanets 50000

  let barePlanets = concatMap (\(x, y, z) -> [x, y, z]) planets

  forM_ (show <$> barePlanets) putStrLn
