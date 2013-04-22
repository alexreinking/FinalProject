{-# LANGUAGE FlexibleInstances #-}
module Main where
import FinalProject.GeneticAlgorithm
import System.Random
import Data.List
import System.IO

-- GA Test 1
initializePool :: Int -> [(Double,Double)]
initializePool size = take size $ zip (randomRs (0,10) (mkStdGen 42)) (randomRs (0,10) (mkStdGen 33))

instance Gene (Double,Double) where
    mutate g (x,y) = (x+(randomRs (-1.0,1.0) g !! 1), y+(randomRs (-1.0,1.0) g !! 2))
    crossover (x1,y1) (x2,y2) = (x1,y2)
    fitness (x,y) = 1/(1+x*x+y*y)

main :: IO ()
main = do
    g <- newStdGen
    putStrLn "Going to evolve a gene to optimize multivariate function!"
    putStr "What should be the size of the gene pool? "
    size <- readLn :: IO Int
    putStr "What should be the mutation rate? "
    mr <- readLn :: IO Double
    putStr "What should be the crossover rate? "
    cr <- readLn :: IO Double
    let xs = take size $ randomRs (-10.0,10.0) g :: [Double]
    let ys = take size $ randomRs (-10.0,10.0) g :: [Double]
    putStr "How many generations would you like to run? "
    n <- readLn :: IO Int
    let result = nthGeneration n mr cr (zip xs ys) g
    putStrLn ("Best chromosome: " ++ show (getBest result))
    putStrLn ("Fitness: " ++ show (fitness $ getBest result))
