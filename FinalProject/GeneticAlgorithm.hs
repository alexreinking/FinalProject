{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}
module FinalProject.GeneticAlgorithm where
import FinalProject.Random
import Foreign.Marshal.Unsafe
import Data.List

class Gene a where
    mutate    :: a -> a
    crossover :: a -> a -> a
    fitness   :: a -> Double

getBest :: (Gene a) => [a] -> a
getBest gs = fst $ last $ getFitness gs
    
getFitness :: (Gene a) => [a] -> [(a,Double)]
getFitness pool = sortBy sortGF $ zip pool (map fitness pool)

nthGeneration :: Gene a => Int -> Double -> Double -> [a] -> [a]
nthGeneration 0 _ _ gp = gp
nthGeneration i mr cr gp = nthGeneration (i - 1) mr cr (nextGeneration mr cr gp)

nextGeneration :: Gene a => Double -> Double -> [a] -> [a]
nextGeneration mr cr gp =
    let poolSize = length gp
        parents = selectParents poolSize gp
        mates = selectParents poolSize gp
        chanceCrossover = (\x y -> if (head (getRandomList 1)) < cr then crossover x y else x)
        offspring = zipWith chanceCrossover parents mates
        chanceMutate = (\x -> if (head (getRandomList 1)) < mr then mutate x else x)
     in map chanceMutate offspring

selectParents :: Gene a => Int -> [a] -> [a]
selectParents i pool =
    let nf = sortBy sortGF $ zip pool (normalizeFitness pool)
        rf =  scanl1 (\(g1,f1) (g2,f2) -> (g2,f1+f2)) nf
        choices = getRandomListRange i (snd $ head rf) (snd $ last rf)
     in map (rouletteSelect rf) choices

rouletteSelect :: Gene a => [(a,Double)] -> Double -> a
rouletteSelect [] _ = error "Bad roulette selection"
rouletteSelect ((g1,f1):gs) r = if (r<f1) then g1 else rouletteSelect gs r

sortGF :: Ord c => (a,c) -> (b,c) -> Ordering
sortGF (g1,f1) (g2,f2)
  | f1 < f2 = LT
  | f1 > f2 = GT
  | f1 == f2 = EQ

normalizeFitness :: Gene a => [a] -> [Double]
normalizeFitness pool = 
    let fs = map fitness pool
        lowest = minimum fs
        fsPos = map (\x -> x + abs lowest) fs
        highest = maximum fsPos
    in map (\x -> x/highest) fsPos
