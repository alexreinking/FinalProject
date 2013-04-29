{-# LANGUAGE Arrows, ScopedTypeVariables, FlexibleInstances #-}
module Main where
import FinalProject.GeneticAlgorithm
import Control.Arrow
import System.Random
import Euterpea

poolSize :: Int
poolSize = 10

_mutate :: StdGen -> Gene (Music Pitch) -> Gene (Music Pitch)
_mutate gen gene = gene { self = transpose (fst $ randomR (-1,1) gen) (self gene) }

--crossover picks a random 16th-note boundary and switches the two
_crossover :: StdGen -> Gene (Music Pitch) -> Gene (Music Pitch) -> Gene (Music Pitch)
_crossover gen g1 g2 =
    let pivots = [0.0,1/16..(min (fromRational $ dur $ self g1) (fromRational $ dur $ self g2))]
        pivot  = pivots !! fst (randomR (0,length pivots - 1) gen)
        part1 = takeM pivot (self g1)
        part2 = dropM pivot (self g2)
     in g1 { self = part1 :+: part2, _geneFit = const 0.0 }
     
initializePool :: Int -> StdGen -> GenePool (Music Pitch)
initializePool size gen = 
    let genes = randomList size (36,60) gen
     in GenePool { pool = map (\x -> Gene { self = note qn (pitch x), _geneFit = const 1.0}) (fst genes),
                   mr = 0.2, cr = 0.8,
                   mutate = _mutate,
                   crossover = _crossover }

runMUI :: StdGen -> GenePool (Music Pitch) -> IO ()
runMUI gen genePool = runUIEx (600,600) "Genetic Music" $
    proc _ -> do
        (_mr, _cr) <- rateSelectors -< ()
        btn <- edge <<< button "Advance Generation" -< ()
        rec let newGenes = [g1,g2,g3,g4,g5,g6,g7,g8,g9,g10]
                newPool = case btn of Nothing -> gp
                                      Just _ -> gp {pool = newGenes, mr = _mr, cr = _cr}
                gp'  = fmap (\_ -> fst $ nextGeneration newPool gen) btn
                gen' = fmap (\_ -> snd $ nextGeneration newPool gen) btn
            gp <- hold genePool -< gp'
            g1 <- getGene -< (pool gp !! 0)
            g2 <- getGene -< (pool gp !! 1)
            g3 <- getGene -< (pool gp !! 2)
            g4 <- getGene -< (pool gp !! 3)
            g5 <- getGene -< (pool gp !! 4)
            g6 <- getGene -< (pool gp !! 5)
            g7 <- getGene -< (pool gp !! 6)
            g8 <- getGene -< (pool gp !! 7)
            g9 <- getGene -< (pool gp !! 8)
            g10 <- getGene -< (pool gp !! 9)
            gen  <- hold gen -< gen'
        title "Last Best" geneDisplay -< getBest newPool

rateSelectors :: (RealFrac a, Show a) => UISF () (a,a)
rateSelectors = leftRight $ proc _ -> do
    _mr <- title "Mutation Rate"  $ withDisplay $ hSlider (0.0,1.0) 0.2 -< ()
    _cr <- title "Crossover Rate" $ withDisplay $ hSlider (0.0,1.0) 0.8 -< ()
    returnA -< (_mr, _cr)

getGene :: UISF (Gene (Music Pitch)) (Gene (Music Pitch))
getGene = leftRight $ proc gene -> do
    title "Gene" display -< self gene
    fit <- title "Fitness" $ withDisplay $ hSlider (0.0,10.0) 5.0 -< ()
    playBtn <- edge <<< button "Play" -< ()
    midiOutB -< (0, fmap (const $ musicToMsgs False [] (toMusic1 $ self gene)) playBtn)
    returnA -< gene { _geneFit = const fit }

geneDisplay :: UISF (Gene (Music Pitch)) ()
geneDisplay = leftRight $ proc gene -> do
    title "Gene" display -< gene
    title "Fitness" display -< fitness gene
    playBtn <- edge <<< button "Play" -< ()
    midiOutB -< (0, fmap (const $ musicToMsgs False [] (toMusic1 $ self gene)) playBtn)
    returnA -< ()

main :: IO ()
main = do
    gen <- newStdGen
    runMUI gen (initializePool poolSize gen)