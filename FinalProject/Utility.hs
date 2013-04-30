module FinalProject.Utility where
import System.Random

-- Utility functions

randomFromList :: RandomGen g => [a] -> g -> (a,g)
randomFromList xs gen =
    let (r,gen') = randomR (0,length xs) gen
     in (xs !! r, gen')

randomList :: (RandomGen g, Random a) => Int -> (a,a) -> g -> ([a],g)
randomList i r g = (fst lst, last (snd lst)) where
    lst = unzip $ take i $ iterate (\(_,g') -> randomR r g') (randomR r g)

-- Unused, but an interesting generalization of randomList.

randomListOf :: (RandomGen g) => Int -> (g -> (a,g)) -> g -> ([a],g)
randomListOf i f g = (fst lst, last (snd lst)) where
    lst = unzip $ take i $ iterate (\(_,g') -> f g') (f g)