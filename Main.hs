{-# LANGUAGE Arrows, ScopedTypeVariables, FlexibleInstances #-}
module Main where
import FinalProject.GeneticAlgorithm
import Control.Arrow
import System.Random
import Euterpea

--Sample GA:

poolSize :: Int
poolSize = 10

_fitness :: Gene (Double, Double) -> Double
_fitness g =
    let (x,y) = self g
     in 1/(1+x^2+y^2)

_mutate :: StdGen -> Gene (Double, Double) -> Gene (Double, Double)
_mutate gen g =
    let ds = fst $ randomList 2 (-1.0,1.0) gen
        (x,y) = self g
     in g { self = (x+(head ds),y+(last ds)) }

_crossover :: StdGen -> Gene (Double, Double) -> Gene (Double, Double) -> Gene (Double, Double)
_crossover _ g1 g2 =
    let (x1,y1) = self g1
        (x2,y2) = self g2
     in g1 { self = (y1,x2) }
     
initializePool :: Int -> StdGen -> GenePool (Double, Double)
initializePool size g = 
    let (r,g') = randomList size (-10.0,10.0) g
        (r',_) = randomList size (-10.0,10.0) g'
        gs = zip r r'
     in GenePool { pool = map (\x -> Gene { self = x, _gene_fit = _fitness}) gs,
                   mr = 0.2, cr = 0.8,
                   mutate = _mutate,
                   crossover = _crossover }
-- MUI

runMUI :: StdGen -> GenePool (Double,Double) -> IO ()
runMUI gen genePool = runUIEx (600,600) "Genetic Music" $
    proc _ -> do
        (_mr, _cr) <- rateSelectors -< ()
        btn <- button "Advance Generation" -< ()
        btnU <- edge -< btn
        rec let gp'  = fmap (\_ -> fst $ nextGeneration (gp {mr = _mr, cr = _cr}) gen) btnU
                gen' = fmap (\_ -> snd $ nextGeneration (gp {mr = _mr, cr = _cr}) gen) btnU
                generation' = fmap (\_ -> generation+(1 :: Integer)) btnU
            gp <- hold genePool -< gp'
            gen  <- hold gen -< gen'
            generation <- hold 0 -< generation'
            best <- hold (getBest genePool) -< mergeE const (fmap (\x -> betterGene (getBest x) best) gp') Nothing
        title "Overall Best" geneDisplay -< best
        title "Current Best" geneDisplay -< getBest gp
        title "Current Generation" display -< generation

betterGene :: Gene a -> Gene a -> Gene a
betterGene g1 g2 = if fitness g1 < fitness g2 then g2 else g1

rateSelectors :: (RealFrac a, Show a) => UISF () (a,a)
rateSelectors = leftRight $ proc _ -> do
    mr <- title "Mutation Rate"  $ withDisplay $ hSlider (0.0,1.0) 0.2 -< ()
    cr <- title "Crossover Rate" $ withDisplay $ hSlider (0.0,1.0) 0.8 -< ()
    returnA -< (mr, cr)

geneDisplay :: (Show a) => UISF (Gene a) ()
geneDisplay = leftRight $ proc gene -> do
    title "Gene" display -< gene
    title "Fitness" display -< fitness gene
    returnA -< ()

main :: IO ()
main = do
    gen <- newStdGen
    runMUI gen (initializePool poolSize gen)