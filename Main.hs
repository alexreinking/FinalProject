{-# LANGUAGE Arrows, ScopedTypeVariables, FlexibleInstances #-}
module Main where
import FinalProject.GeneticAlgorithm
import Control.Arrow
import System.Random
import Euterpea

poolSize :: Int
poolSize = 100

instance Gene (Double,Double) where
    mutate gen (x,y) = (x+(randomRs (-1.0,1.0) gen !! 1), y+(randomRs (-1.0,1.0) gen !! 2))
    crossover (x1,_) (_,y2) = (x1,y2)
    fitness (x,y) = 1/(1+x^(2 :: Integer)+y^(2 :: Integer)) -- This supresses a GHC warning. Why does this even throw a warning?

runMUI :: StdGen -> [(Double,Double)] -> IO ()
runMUI gen pool = runUIEx (600,600) "Genetic Music" $
    proc _ -> do
        (mr, cr) <- rateSelectors -< ()
        btn <- button "Advance Generation" -< ()
        btnU <- edge -< btn
        rec let ap' = fmap (\_ -> fst $ nextGeneration mr cr ap gen) btnU
                gen'  = fmap (\_ -> snd $ nextGeneration mr cr ap gen) btnU
                generation' = fmap (\_ -> generation+(1 :: Integer)) btnU
            ap <- hold pool -< ap'
            gen  <- hold gen -< gen'
            generation <- hold 0 -< generation'
            best <- hold (getBest pool) -< mergeE const (fmap (\x -> betterGene (getBest x) best) ap') Nothing
        title "Overall Best" geneDisplay -< best
        title "Current Best" geneDisplay -< getBest ap
        title "Current Generation" display -< generation

betterGene :: Gene a => a -> a -> a
betterGene g1 g2 = if fitness g1 < fitness g2 then g2 else g1

rateSelectors :: (RealFrac a, Show a) => UISF () (a,a)
rateSelectors = leftRight $ proc _ -> do
    mr <- title "Mutation Rate"  $ withDisplay $ hSlider (0.0,1.0) 0.2 -< ()
    cr <- title "Crossover Rate" $ withDisplay $ hSlider (0.0,1.0) 0.8 -< ()
    returnA -< (mr, cr)

geneDisplay :: (Gene a, Show a) => UISF a ()
geneDisplay = leftRight $ proc gene -> do
    title "Gene" display -< gene
    title "Fitness" display -< fitness gene
    returnA -< ()

main :: IO ()
main = do
    gen <- newStdGen
    let xs = take poolSize $ randomRs (5.0,10.0) gen :: [Double]
    let ys = take poolSize $ randomRs (5.0,10.0) gen :: [Double]
    runMUI gen (zip xs ys)