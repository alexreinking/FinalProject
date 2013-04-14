module FinalProject.Random (getRandomList,getRandomListRange) where
import System.Random
import Foreign.Marshal.Unsafe

-- The purpose of this module is to remove the need for monads in random numbers
-- by forcing eager evaluation on the results of getRandomList/getRandomListRange,
-- we can be sure that the function isn't called too many times.

-- Additionally, since unsafeLocalState is meant to be used for mathematical functions,
-- it is reasonable to view the getRandom function as a PRNG and to use getRandomList/
-- getRandomListRange as making calls to the PRNG and increasing the state behind-the-
-- scenes.

getRandom :: IO (Double)
getRandom = do
    r <- randomIO :: IO (Double)
    return (r)

getRandomList :: Integer -> [Double]
getRandomList x = map (\x -> unsafeLocalState $ getRandom) [1..x]

getRandomListRange :: (Enum a, Num a) => a -> Double -> Double -> [Double]
getRandomListRange x low high =
    let range = high-low
     in map (\x -> low+range*(unsafeLocalState $ getRandom)) [1..x]