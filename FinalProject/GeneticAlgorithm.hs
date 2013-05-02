{-# LANGUAGE FlexibleInstances #-}
module FinalProject.GeneticAlgorithm where
import FinalProject.Utility
import System.Random
import Data.List
import Data.Ord

-- Data declarations
    
data GenePool a = GenePool {
    pool :: [Gene a],
    mr :: Double,
    cr :: Double,
    mutate    :: StdGen -> Gene a -> Gene a,
    crossover :: StdGen -> Gene a -> Gene a -> Gene a
}

data Gene a = Gene {
    self      :: a,
    _geneFit :: Gene a -> Double -- this is useful for passing a closure from IO/MUI
}

-- Showable instances

instance (Show a) => Show (Gene a) where
    show g = "Gene {" ++ show (self g) ++ ", " ++ show (fitness g) ++ "}"
    
instance (Show a) => Show (GenePool a) where
    show gp = "GenePool {" ++ show (pool gp) ++ "}"

-- This makes the syntax more readable

fitness :: Gene a -> Double
fitness g = _geneFit g g

-- This function HAD been used, but I worked around it. I'm leaving it in because it could be useful in other contexts.
freezeFitness :: Gene a -> Gene a
freezeFitness g = g { _geneFit = const (fitness g) }

getBest :: GenePool a -> Gene a
getBest gs = fst $ last $ getFitness gs

getFitness :: GenePool a -> [(Gene a,Double)]
getFitness gp = sortBy (comparing snd) $ zip (pool gp) (map fitness (pool gp))

-- This function is never used, but was easy to write and would be useful for any
-- fully-automated genetic algorithm
nthGeneration :: Int -> GenePool a -> StdGen -> GenePool a
nthGeneration i gp g = last $ fst $ unzip $ take i $
    iterate (uncurry nextGeneration) (nextGeneration gp g)

nextGeneration :: GenePool a -> StdGen -> (GenePool a, StdGen)
nextGeneration gp g =
    let poolSize = length (pool gp)
        (parents, g1) = selectParents gp g
        (mates, g2) = selectParents gp g1
        (g3,g4) = split g2
        chanceCrossover r x y = if r < cr gp then crossover gp g2 x y else x
        chanceMutate r x = if r < mr gp then mutate gp g3 x else x
        (rs, g5) = randomList poolSize (0.0,1.0) g4
        offspring = zipWith3 chanceCrossover rs parents mates
        (rs', g6) = randomList poolSize (0.0,1.0) g5
     in (gp { pool = zipWith chanceMutate rs' offspring }, g6)

selectParents :: GenePool a -> StdGen -> ([Gene a], StdGen)
selectParents gp g = 
    let poolSize = length $ pool gp
        (rands,g') = randomList poolSize (0.0,1.0) g
     in (map (rouletteSelect gp) rands, g')

rouletteSelect :: GenePool a -> Double -> Gene a
rouletteSelect gp r = fst $ head $ filter ((>= r).snd) (normalizeFitness gp)

normalizeFitness :: GenePool a -> [(Gene a, Double)]
normalizeFitness gp = 
    let (genes,fs) = unzip $ getFitness gp
        norm = map (/ sum fs) fs
        newFs = scanl1 (+) norm
     in zip genes newFs
