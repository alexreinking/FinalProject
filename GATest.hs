{-# LANGUAGE FlexibleInstances #-}
module GATest where
import FinalProject.GeneticAlgorithm
import System.Random
import Data.List
import System.IO

saveShowable :: Show a => a -> IO ()
saveShowable arr = do
    outH <- openFile "ga.txt" WriteMode
    hPrint outH arr
    hClose outH

-- GA Test 1
initializePool :: Int -> [(Double,Double)]
initializePool size = take size $ zip (randomRs (0,10) (mkStdGen 42)) (randomRs (0,10) (mkStdGen 33))

instance Gene (Double,Double) where
    mutate g (x,y) = (x+(randomRs (-1.0,1.0) g !! 1), y+(randomRs (-1.0,1.0) g !! 2))
    crossover (x1,y1) (x2,y2) = (x1,y2)
    fitness (x,y) = 1/(1+x*x+y*y)
