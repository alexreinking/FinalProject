{-# LANGUAGE FlexibleInstances #-}
module GATest where
import FinalProject.Random
import FinalProject.GeneticAlgorithm
import Foreign.Marshal.Unsafe
import Data.List
import System.IO

saveShowable :: Show a => a -> IO ()
saveShowable arr = do
    outH <- openFile "ga.txt" WriteMode
    hPrint outH arr
    hClose outH

-- GA Test 1
initializePool :: Int -> [(Double,Double)]
initializePool size = zip (getRandomListRange size 0 10) (getRandomListRange size 0 10)

instance Gene (Double,Double) where
    mutate (x,y) = (x+(head $ getRandomListRange 1 (-1) 1),y+(head $ getRandomListRange 1 (-1) 1))
    crossover (x1,y1) (x2,y2) = (x1,y2)
    fitness (x,y) = 1/(1+x*x+y*y)

-- GA Test 2
 
initializeInteractivePool :: Int -> [Double]
initializeInteractivePool size = getRandomListRange size 0 10

askForFitness :: Double -> Double
askForFitness x = unsafeLocalState $
    do putStrLn ("How much do you like " ++ (show x) ++ "?")
       val <- (readLn :: IO Double)
       return (val)

instance Gene Double where
    mutate x = x + head (getRandomListRange 1 (-1) 1)
    crossover x y = (x+y)/2
    fitness x = askForFitness x