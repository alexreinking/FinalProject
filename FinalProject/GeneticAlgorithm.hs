module FinalProject.GeneticAlgorithm where
import System.Random
import Control.Monad.State
import Data.List
import Data.Ord

class Gene a where
    mutate    :: StdGen -> a -> a
    crossover :: a -> a -> a
    fitness   :: a -> Double

getBest :: (Gene a) => [a] -> a
getBest gs = fst $ last $ getFitness gs
    
getFitness :: (Gene a) => [a] -> [(a,Double)]
getFitness pool = sortBy (comparing snd) $ zip pool (map fitness pool)

nthGeneration :: Gene a => Int -> Double -> Double -> [a] -> StdGen -> [a]
nthGeneration 0 _ _ gp _ = gp
nthGeneration i mr cr gp g = 
    let (n,g') = nextGeneration mr cr gp g
     in nthGeneration (i - 1) mr cr n g'

nextGeneration :: Gene a => Double -> Double -> [a] -> StdGen -> ([a], StdGen)
nextGeneration mr cr gp g =
    let poolSize = length gp
        (parents,g1) = selectParents poolSize gp g
        (mates,g2) = selectParents poolSize gp g1
        (g3,g4) = split g2
        chanceCrossover r x y = if r < cr then crossover x y else x
        offspring = zipWith3 chanceCrossover (randomRs (0.0,1.0) g2) parents mates
        chanceMutate r x = if r < mr then mutate g3 x else x
     in (zipWith chanceMutate (drop poolSize $ randomRs (0.0,1.0) g3) offspring, g4)

selectParents :: Gene a => Int -> [a] -> StdGen -> ([a],StdGen)
selectParents 0 _ g = ([],g)
selectParents i pool g = 
    let nf = sortBy (comparing snd) $ zip pool (normalizeFitness pool)
        rf =  scanl1 (\(_,f1) (g2,f2) -> (g2,f1+f2)) nf
        (r,g') = randomR (0.0,1.0) g
        choice = r*snd (last rf)-snd (head rf)*(1-r)
     in (rouletteSelect rf choice : fst (selectParents (i-1) pool g'),g')

rouletteSelect :: Gene a => [(a,Double)] -> Double -> a
rouletteSelect [] _ = error "Bad roulette selection"
rouletteSelect ((g1,f1):gs) r = if r<f1 then g1 else rouletteSelect gs r

normalizeFitness :: Gene a => [a] -> [Double]
normalizeFitness pool = 
    let fs = map fitness pool
        lowest = minimum fs
        fsPos = map (\x -> x + abs lowest) fs
        highest = maximum fsPos
     in map (/ highest) fsPos
