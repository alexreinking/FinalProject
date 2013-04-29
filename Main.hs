{-# LANGUAGE Arrows, ScopedTypeVariables, FlexibleInstances #-}
module Main where
import FinalProject.GeneticAlgorithm
import Euterpea.Examples.RandomMusic
import Euterpea.Examples.LSystems
import Data.MarkovChain as M
import Control.Arrow
import System.Random
import Euterpea

totalDur :: Dur
totalDur = 4*wn -- 4 bars

_mutate :: StdGen -> Gene (Music Pitch) -> Gene (Music Pitch)
_mutate gen gene = gene { self = transpose (fst $ randomR (-1,1) gen) (self gene) }

_crossover :: StdGen -> Gene (Music Pitch) -> Gene (Music Pitch) -> Gene (Music Pitch)
_crossover gen g1 g2 =
    let pivots = [0.0,1/16..(min (fromRational $ dur $ self g1) (fromRational $ dur $ self g2))]
        pivot  = pivots !! fst (randomR (0,length pivots - 1) gen)
        part1 = takeM pivot (self g1)
        part2 = dropM pivot (self g2)
     in g1 { self = part1 :+: part2, _geneFit = const 0.0 }

-- Sonata in C Major (Mozart)

sonataInC :: [Music Pitch]
sonataInC = [c 5 wn, e 5 hn, g 5 hn, b 4 dhn, c 5 en, d 5 en, c 5 hn, rest hn, 
             a 5 wn, g 5 hn, c 6 hn, g 5 hn, f 5 en, g 5 en, e 5 en, f 5 en, e 5 hn, rest hn, 
             a 4 qn, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en, g 5 en, a 5 en,
             g 5 en, f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en,
             g 4 qn, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en, g 5 en,
             f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en,
             f 4 qn, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en,
             e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en, f 4 en,
             e 4 qn, f 4 en, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, 
             d 5 en, c 5 en, b 4 en, a 4 en, g 4 en, f 4 en, e 4 en,
             d 4 qn, e 4 en, f 4 en, g 4 en, a 4 en, b 4 en, cs 5 en,
             d 5 en, a 4 en, b 4 en, cs 5 en, d 5 en, e 5 en, f 5 en, g 5 en,
             a 5 en, b 5 en, c 6 en, b 5 en, a 5 en, g 5 en, f 5 en, e 5 en,
             f 5 en, g 5 en, a 5 en, g 5 en, f 5 en, e 5 en, d 5 en, c 5 en,
             b 4 qn, g 5 qn, e 5 qn, c 5 qn, d 5 qn, g 5 qn, e 5 qn, c 5 qn,
             d 5 hn, g 5 hn, g 4 hn, rest hn]

-- Sonatina (Clementi)

sonatina :: [Music Pitch]
sonatina = [c 5 qn, e 5 en, c 5 en, g 4 qn, g 4 qn, c 5 qn, e 5 en, c 5 en, g 4 qn, g 5 qn,
            f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, c 5 en, b 4 en, c 5 en, d 5 en, c 5 en,
            b 4 en, a 4 en, g 4 qn, rest qn, c 5 qn, e 5 en, c 5 en, g 4 qn, g 4 qn,
            e 5 qn, g 5 en, e 5 en, c 5 qn, e 5 en, c 5 en, d 5 en, b 4 en, c 5 en, a 4 en,
            b 4 en, g 4 en, a 4 en, fs 4 en, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en,
            fs 5 en, g 5 en, a 4 qn, a 5 qn, a 5 qn, a 5 qn, b 4 en, c 5 en, d 5 en, e 5 en,
            fs 5 en, g 5 en, a 5 en, b 5 en, c 5 qn, c 6 qn, c 6 qn, c 6 qn, d 5 en, g 5 en,
            b 5 en, d 6 en, c 6 en, b 5 en, a 5 en, g 5 en, fs 5 en, e 5 en, g 5 en, fs 5 en,
            a 5 en, g 5 en, fs 5 en, e 5 en, e 5 en, d 5 en, c 5 en, b 4 en, d 5 en, c 5 en,
            b 4 en, a 4 en, g 4 hn, rest hn]

-- Sonata No. 7 (Haydn)

sonataNo7 :: [Music Pitch]
sonataNo7 = [g 4 qn, g 4 hn, e 4 qn, c 5 qn, c 5 hn, b 4 qn, d 5 qn, d 5 dqn, e 5 en, f 5 qn,
             f 5 qn, f 5 hn, e 5 qn, g 5 qn, g 5 dhn, gs 5 qn, a 5 dqn, b 5 en, d 6 en, c 6 en,
             b 5 en, a 5 en, g 5 hn, fs 5 en, g 5 en, e 5 en, fs 5 en, g 5 dhn, g 4 qn, g 4 hn,
             e 4 qn, c 5 qn, c 5 hn, b 4 qn, d 5 qn, d 5 dqn, e 5 en, f 5 qn, f 5 qn, f 5 hn,
             e 5 qn, g 5 qn, g 5 dhn, gs 5 qn, a 5 dqn, b 5 en, d 6 en, c 6 en, b 5 en, a 5 en,
             g 5 hn, fs 5 en, g 5 en, e 5 en, fs 5 en, g 5 dhn, g 5 qn]

-- initialize from t2
     
initializePool :: Int -> StdGen -> GenePool (Music Pitch)
initializePool size gen = 
    let gens = take size (iterate (fst . split) gen)
        genes = map (M.runMulti 1 [sonataNo7,sonatina,sonataInC] 0) gens
     in GenePool { pool = map (\x -> Gene { self = takeM totalDur (line (concat x)), _geneFit = const 1.0}) genes,
                   mr = 0.2, cr = 0.8,
                   mutate = _mutate,
                   crossover = _crossover }

runMUI :: StdGen -> GenePool (Music Pitch) -> IO ()
runMUI gen genePool = runUIEx (600,700) "Genetic Music" $
    proc _ -> do
        (_mr, _cr) <- rateSelectors -< ()
        btn <- edge <<< button "Advance Generation" -< ()
        rec let newGenes = [g1,g2,g3,g4,g5,g6,g7,g8,g9,g10]
                newPool = case btn of Nothing -> gp
                                      Just _ -> gp {pool = newGenes, mr = _mr, cr = _cr}
                gp'  = fmap (\_ -> fst $ nextGeneration newPool gen) btn
                gen' = fmap (\_ -> snd $ nextGeneration newPool gen) btn
            gp  <- hold genePool -< gp'
            g1  <- setFitness -< (pool gp !! 0)
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
        title "Last Best" geneDisplay -< getBest newPool

performGene :: Gene (Music Pitch) -> Music1
performGene gene = toMusic1 $ tempo (4/3) (self gene)

rateSelectors :: (RealFrac a, Show a) => UISF () (a,a)
rateSelectors = leftRight $ proc _ -> do
    _mr <- title "Mutation Rate"  $ withDisplay $ hSlider (0.0,1.0) 0.2 -< ()
    _cr <- title "Crossover Rate" $ withDisplay $ hSlider (0.0,1.0) 0.8 -< ()
    returnA -< (_mr, _cr)

setFitness :: UISF (Gene (Music Pitch)) (Gene (Music Pitch))
setFitness = leftRight $ proc gene -> do
    fit <- title "Fitness" $ hSlider (0.0,10.0) 5.0 -< ()
    playBtn <- title "Actions" $ edge <<< button "Play" -< ()
    midiOutB -< (0, fmap (const $ musicToMsgs False [] (performGene gene)) playBtn)
    returnA -< gene { _geneFit = const fit }

geneDisplay :: UISF (Gene (Music Pitch)) ()
geneDisplay = leftRight $ proc gene -> do
    title "Gene" display -< gene
    title "Fitness" display -< fitness gene
    playBtn <- edge <<< button "Play" -< ()
    midiOutB -< (0, fmap (const $ musicToMsgs False [] (performGene gene)) playBtn)
    returnA -< ()

main :: IO ()
main = do
    gen <- newStdGen
    runMUI gen (initializePool 10 gen)