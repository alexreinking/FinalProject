{-# LANGUAGE Arrows, ScopedTypeVariables, FlexibleInstances #-}
module Main where
import FinalProject.GeneticAlgorithm
import FinalProject.Utility
import FinalProject.Sample
import qualified Data.List as L
import Data.MarkovChain as M
import Control.Arrow
import System.Random
import Euterpea

-- Global Parameters

sample :: [Music Pitch]
sample = concat [sonataInC, sonatina, sonataNo7]

-- Genetic Algorithm

_mutate :: StdGen -> Gene [Music Pitch] -> Gene [Music Pitch]
_mutate gen gene =
    let neighbors = map (\x -> x : getNeighbors x sample) (self gene)
        (indices,_) = randomList (length neighbors) (0,maximum (map length neighbors)) gen
     in gene { self = zipWith (\x y -> y !! (x `mod` length y)) indices neighbors }

getNeighbors :: Music Pitch -> [Music Pitch] -> [Music Pitch]
getNeighbors mp mps = L.nub $ map ((mps !!).(+1).snd) $ filter ((mp ==).fst) $ zip mps [0..length mps-2]

_crossover :: StdGen -> Gene [Music Pitch] -> Gene [Music Pitch] -> Gene [Music Pitch]
_crossover gen g1 g2 =
    let pivot  = fst (randomR (0,min (length (self g1)-1) (length (self g2)-1)) gen)
        part1 = take pivot (self g1)
        part2 = drop pivot (self g2)
     in g1 { self = part1 ++ part2, _geneFit = const 0.0 }
     
initializePool :: Int -> StdGen -> GenePool [Music Pitch]
initializePool size gen = 
    let gens = take size (iterate (fst . split) gen)
        starts = map (fst . randomR (0, 2)) gens
        genes = zipWith (M.run 1 sample) starts gens
     in GenePool { pool = map (\x -> Gene { self = take 20 x, _geneFit = const 0.0}) genes,
                   mr = 0.2, cr = 0.8,
                   mutate = _mutate,
                   crossover = _crossover }

-- MUI

runMUI :: StdGen -> GenePool [Music Pitch] -> IO ()
runMUI gen genePool = runUIEx (600,800) "Genetic Music" $
    proc _ -> do
        (_mr, _cr) <- rateSelectors -< ()
        btn <- edge <<< button "Advance Generation" -< ()
        rec let newPool =
                    case btn of Nothing -> gp
                                Just _ -> gp {pool = [g1,g2,g3,g4,g5,g6,g7,g8,g9,g10],
                                              mr = _mr,
                                              cr = _cr }
                gp'  = fmap (\_ -> fst $ nextGeneration newPool gen) btn
                gen' = fmap (\_ -> snd $ nextGeneration newPool gen) btn
                sup' = fmap (\_ -> getBest newPool) btn
            sup <- hold Gene { self = [rest 0], _geneFit = const 0.0 } -< sup'
            gp  <- hold genePool -< gp'
            g1  <- setFitness -< head (pool gp)
            g2  <- setFitness -< (pool gp !! 1)
            g3  <- setFitness -< (pool gp !! 2)
            g4  <- setFitness -< (pool gp !! 3)
            g5  <- setFitness -< (pool gp !! 4)
            g6  <- setFitness -< (pool gp !! 5)
            g7  <- setFitness -< (pool gp !! 6)
            g8  <- setFitness -< (pool gp !! 7)
            g9  <- setFitness -< (pool gp !! 8)
            g10 <- setFitness -< (pool gp !! 9)
            gen <- hold gen   -< gen'
        title "Last Best" geneDisplay -< sup

-- Custom Widgets

rateSelectors :: (RealFrac a, Show a) => UISF () (a,a)
rateSelectors = leftRight $ proc _ -> do
    _mr <- title "Mutation Rate"  $ withDisplay $ hSlider (0.0,1.0) 0.2 -< ()
    _cr <- title "Crossover Rate" $ withDisplay $ hSlider (0.0,1.0) 0.8 -< ()
    returnA -< (_mr, _cr)

setFitness :: UISF (Gene [Music Pitch]) (Gene [Music Pitch])
setFitness = leftRight $ proc gene -> do
    fit <- title "Fitness" $ hSlider (0.0,10.0) 5.0 -< ()
    playBtn <- title "Actions" $ edge <<< button "Play" -< ()
    midiOutB -< (0, fmap (const $ musicToMsgs False [] (performGene gene)) playBtn)
    returnA -< gene { _geneFit = const fit }

geneDisplay :: UISF (Gene [Music Pitch]) ()
geneDisplay = leftRight $ proc gene -> do
    title "Gene" display -< gene
    title "Fitness" display -< fitness gene
    playBtn <- title "Actions" $ edge <<< button "Play" -< ()
    midiOutB -< (0, fmap (const $ musicToMsgs False [] (performGene gene)) playBtn)
    returnA -< ()

-- Widget Helpers

performGene :: Gene [Music Pitch] -> Music1
performGene gene = toMusic1 $ tempo (4/3) (line $ self gene)

-- Main

main :: IO ()
main = do
    gen <- newStdGen
    runMUI gen (initializePool 10 gen)