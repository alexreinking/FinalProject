{-# LANGUAGE Arrows, ScopedTypeVariables, FlexibleInstances #-}
module Main where
import FinalProject.GeneticAlgorithm
import Euterpea.Examples.MUI
import Control.Arrow
import System.Random
import Data.Maybe
import System.IO
import Data.List
import Euterpea

poolSize :: Int
poolSize = 100

instance Gene (Double,Double) where
    mutate g (x,y) = (x+(randomRs (-1.0,1.0) g !! 1), y+(randomRs (-1.0,1.0) g !! 2))
    crossover (x1,y1) (x2,y2) = (x1,y2)
    fitness (x,y) = 1/(1+x^2+y^2)

runMUI :: StdGen -> [(Double,Double)] -> IO ()
runMUI g pool = runUIEx (600,600) "Genetic Music" $
    proc _ -> do
        devId <- selectOutput -< ()
        (mr, cr) <- rateSelectors -< ()
        btn <- button "Advance Generation" -< ()
        btnU <- edge -< btn
        rec let ap' = fmap (\x -> fst $ nextGeneration mr cr ap g) btnU
                g'  = fmap (\x -> snd $ nextGeneration mr cr ap g) btnU
                gn' = fmap (\x -> gn+1) btnU
            ap <- hold pool -< ap'
            g  <- hold g -< g'
            gn <- hold 0 -< gn'
            best <- hold (getBest pool) -< mergeE const (fmap (\x -> betterGene (getBest x) best) ap') Nothing
        title "Overall Best" geneDisplay -< best
        title "Current Best" geneDisplay -< getBest ap
        title "Current Generation" display -< gn

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

main = do
    g <- newStdGen
    let xs = take poolSize $ randomRs (5.0,10.0) g :: [Double]
    let ys = take poolSize $ randomRs (5.0,10.0) g :: [Double]
    runMUI g (zip xs ys)