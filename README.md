=================================

     Genetic Melody Composer     
         Alex Reinking

           CPSC  431
          Spring 2013

=================================

This project aims to create small musical phrases or ideas, initially four bars long, that are tailored to fit the user's tastes.

It operates by default in the classical style since it is trained on three famous classical movements:

1. The Exposition of Sonata in C Major by Mozart
2. Sonatina Op. 36, No 1. by Clementi
3. Sonata No. 7 by Haydn

The pieces were chosen for both their quality and their key: all three are in C major, though they borrow chords from related keys.

The interactive step is conducted by setting sliders that are translated into fitness values for a genetic algorithm on a fixed-size gene pool of size ten. The pool is initialized via a Markov Chain of second-order connectivity.


How-To
------
Load the program in GHCi or compile Main.hs with GHC and run main.

Listen to each sample melody, set the fitness and then click on advance generation. Repeat until you have a result you like.
I can usually get something unique and halfway decent after about 8 generations. I've found myself humming a few examples, actually.

The option exists to tweak the mutation rate and crossover rate. The changes will take effect as you press the "Advance Generation" button.

Hope you have a little fun with this! :)

Code Overview
-------------

The code is split into four files:

1) Utility.hs: This file contains two useful functions:
    a) `randomFromList :: RandomGen g => [a] -> g -> (a,g)`

       This function picks an element at random out of a list and returns it along with the modified generator.

    b) `randomList :: (RandomGen g, Random a) => Int -> (a,a) -> g -> ([a],g)`

       This function creates a list of random numbers of a specific size and returns the updated generator. I wrote this because it is difficult to predict ahead of time how many random numbers will be needed and randomRs just returns an infinite list.

       I preferred to string the generator through, rather than the list.

2) Sample.hs: This file contains the melodies to the aforementioned pieces. They are expressed as arrays of notes (Music Pitch values) and are valid inputs to both the genetic algorithm and the Euterpea function `line`

3) GeneticAlgorithm.hs: This file implements a general genetic algorithm
    Two data declarations are used as inputs to all the GA functions:
    a) `data Gene a`:
        This structure holds a value of type `a` as well as a function to evaluate its fitness. This is to allow the GA library to be general enough to allow for unconventional fitness functions. Genes can be assigned different fitness functions in a single gene pool and constant functions originating from user input may be passed. Additionally, it is possible to replace the fitness function with its calculation via the `freezeFitness` function.
    b) `data GenePool a`:
       This structure contains a list of `Gene a`'s, the pool as well as `Double`s representing the mutation rate and crossover rate. The gene pool also carries with it functions that mutate a single gene and that cross two genes: `mutate :: StdGen -> Gene a -> Gene a` and `crossover :: StdGen -> Gene a -> Gene a -> Gene a`

    The other functions in GeneticAlgorithm.hs are:

    a) `fitness :: Gene a -> Double`:
       This evaluates a particular gene's fitness function on itself and returns the value

    b) `getBest :: GenePool a -> Gene a`:
       Simply retrieves the gene with highest fitness in its argument.

    c) `getFitness :: GenePool a -> [(Gene a, Double)]`:
       Returns pairs of genes along with their evaluated fitnesses in ascending order. Used by other functions.

    d) `nthGeneration :: Int -> GenePool a -> StdGen -> GenePool a`
       Runs `nextGeneration` n times on its argument. Handles the random number generator such that `nextGeneration` never starts with the same generator.

    e) `nextGeneration :: GenePool a -> StdGen -> (GenePool a, StdGen)`
       This function advances the gene pool to the next generation. It calls the select parents function twice to get two equally-sized lists. For each pair, it determines at random whether or not to cross them over. Once their offspring are produced, each one is mutated at random. The random number generator is strung along as this happens and an unused generator is returned along with the advanced gene pool.

    f) `selectParents :: GenePool a -> StdGen -> ([Gene a], StdGen)`
       Generates a list of random numbers between 0.0 and 1.0 equal in size to the gene pool and maps the function `rouletteSelect` to it. 

    g) `rouletteSelect :: GenePool a -> Double -> Gene a`
        This routine unsurprisingly uses roulette-wheel selection to choose a gene from the pool. It assumes that the number it is passed lies in the closed interval [0.0,1.0].

    h) `normalizeFitness :: GenePool a -> [(Gene a, Double)]`
        This function scales back all of the fitnesses such that their sum is one and then scans the sum function down the list. It is a helper function to `rouletteSelect` and is what allows roulette selection to work.

        For example, if the genes in the gene pool have fitnesses: 2, 2, and 2, then each will be reassigned to: 1/3, 2/3 and 1, respectively. Then, for some random number between 0 and 1, each has an equal chance of being the lowest number above the random number. That is, the adjusted fitnesses are equivalent to fractions on a pie chart, with the highest fitnesses having the largest slices of the pie.

4) Main.hs:
    Main contains three things: The specific Genetic Algorithm for this problem, the MUI, and the main function.

    a) The Genetic Algorithm:
       The mutation function, specified by `_mutate`, works as follows:

       For each note in the sequence, it consults the sample for the set of notes that neighbor it. Then, the note is substituted for a random element of that set or itself all with equal probability. This mutation function was chosen in order to keep a sense of key in the pieces, while still changing the melodies and being informed by the data.

       The crossover function (specified by `_crossover`) is simpler:

       Crossover simply selects a pivot in the smaller list, takes the beginning of the smaller list and merges it with the second list minus the first pivot elements.

       Finally, `initializePool :: Int -> StdGen -> GenePool [Music Pitch]` uses the 2-connected Markov Chain procedure to create a pool of melodies with a few different starting pitches as candidates. Although the lists it creates are longer than four bars, the MUI will only play back the first four bars.

    b) The MUI:
       The MUI is fairly straight-forward. It responds to button clicks and constructs the new gene pool (along with user-submitted fitnesses) before advancing to the next generation using the functions in GeneticAlgorithm.hs. A small number of custom widgets are composed out of existing components and serve mostly to give the MUI a reasonable layout.

       To work with the MUI, play each Gene and set the slider to indicate how much you like the phrase. Lower is worse and higher is better.

       The best gene from the last generation is stored and displayed at the bottom and provides the option of being saved to a file.

    c) Main function:
       This one isn't exciting at all. It gets a random number generator from the OS and passes an initial pool of ten genes to the MUI.

...and there you have it! That's how the code works.

There are a few functions that I wrote that I ultimately ended up not using. However, I have left them in the code because they could be useful on their own or could be used in other contexts.

The latest version and all of my progress will be permanently available on https://www.github.com/alexreinking/FinalProject

Going Forward
-------------

There's a lot of neat stuff I could have done if I'd had more time. Here are a few of my ideas:

1) Simultaneously train a neural net to rate the fitnesses for you and then let the GA run automatically
2) Try to derive rules for a grammar from the training data.
  a) Add more training data. More data is always better for something like this.
3) Try different gene representations (ie. use intervals instead of notes)
4) Try different ways of initializing the pool. (If we start with something already-composed, we can evolve variations)