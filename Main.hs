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
        starts = map (fst . randomR (0, 5)) gens
        genes = zipWith (M.run 2 sample) starts gens
     in GenePool { pool = map (\x -> Gene { self = take 40 x, _geneFit = const 0.0}) genes,
                   mr = 0.2, cr = 0.8,
                   mutate = _mutate,
                   crossover = _crossover }

-- MUI

runMUI :: StdGen -> GenePool [Music Pitch] -> IO ()
runMUI gen genePool = runUIEx (930,550) "Genetic Music" $
    proc _ -> do
        (_mr, _cr) <- rateSelectors -< ()
        btn <- edge <<< button "Advance Generation" -< ()
        rec let newPool =
                    case btn of Nothing -> gp
                                Just _ -> gp {pool = part1 ++ part2,
                                              mr = _mr, cr = _cr }
                gp'  = fmap (\_ -> fst $ nextGeneration newPool gen) btn
                gen' = fmap (\_ -> snd $ nextGeneration newPool gen) btn
                sup' = fmap (\_ -> getBest newPool) btn
                gener' = fmap (\_ -> gener+ (1 :: Integer)) btn
            sup   <- hold Gene { self = [rest 0], _geneFit = const 0.0 } -< sup'
            gp    <- hold genePool -< gp'
            part1 <- get5Genes     -< take 5 (pool gp)
            part2 <- get5Genes     -< drop 5 (pool gp)
            gen   <- hold gen      -< gen'
            gener <- hold 0        -< gener'
        title "Last Best" geneDisplay -< sup
        title "Generations" display   -< gener

-- Custom Widgets

-- I don't know of a better way to do this. But it works.
-- I'm open to suggestions!
get5Genes :: UISF [Gene [Music Pitch]] [Gene [Music Pitch]]
get5Genes = leftRight $ proc genes -> do
    g0 <- title "Gene" setFitness -< head genes
    g1 <- title "Gene" setFitness -< genes !! 1
    g2 <- title "Gene" setFitness -< genes !! 2
    g3 <- title "Gene" setFitness -< genes !! 3
    g4 <- title "Gene" setFitness -< genes !! 4
    returnA -< [g0,g1,g2,g3,g4]

rateSelectors :: (RealFrac a, Show a) => UISF () (a,a)
rateSelectors = leftRight $ proc _ -> do
    _mr <- title "Mutation Rate"  $ withDisplay $ hSlider (0.0,1.0) 0.3 -< ()
    _cr <- title "Crossover Rate" $ withDisplay $ hSlider (0.0,1.0) 0.7 -< ()
    returnA -< (_mr, _cr)

setFitness :: UISF (Gene [Music Pitch]) (Gene [Music Pitch])
setFitness = proc gene -> do
    fit <- title "Fitness" $ hSlider (0.0,10.0) 5.0 -< ()
    playBtn <- title "Actions" $ edge <<< button "Play" -< ()
    midiOutB -< (0, fmap (const $ musicToMsgs False [] (performGene gene)) playBtn)
    returnA -< gene { _geneFit = const fit }

geneDisplay :: UISF (Gene [Music Pitch]) ()
geneDisplay = leftRight $ proc gene -> do
    title "Gene" display -< gene
    title "Fitness" display -< fitness gene
    btn <- title "Actions" playSave -< ()
    --playBtn <- title "Actions" $ edge <<< button "Play" -< ()
    let playAction x = case x of Nothing -> Nothing
                                 Just "Play" -> Just (musicToMsgs False [] (performGene gene))
                                 _ -> Nothing
    let saveAction x = case x of Nothing -> Nothing
                                 Just "Save" -> Just (takeM 4 $ line $ self gene)
                                 _ -> Nothing
    midiOutB -< (0, playAction btn)
    basicIOWidget1 test -< saveAction btn
    returnA -< ()

basicIOWidget1 :: (a -> IO ()) -> UISF (SEvent a) () 
basicIOWidget1 = (>>> arr (const ())) . uisfSinkE

playSave :: UISF () (SEvent String)
playSave = leftRight $ proc _ -> do
    playBtn <- edge <<< button "Play" -< ()
    saveBtn <- edge <<< button "Save to test.mid" -< ()
    returnA -< case playBtn of Just _ -> Just "Play"
                               Nothing -> case saveBtn of Just _ -> Just "Save"
                                                          Nothing -> Nothing

-- Widget Helpers

performGene :: Gene [Music Pitch] -> Music1
performGene gene = toMusic1 $ instrument Xylophone $ tempo 2 (takeM 4 $ line $ self gene)

-- Main

main :: IO ()
main = do
    gen <- newStdGen
    runMUI gen (initializePool 10 gen)