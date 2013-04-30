CPSC 431 Final Project
======================

My CPSC 431 Final Project at Yale University.

The idea is to genetically evolve a rules-based system that the user likes. The interactive period can take some time to conduct, but with a bit of dedication could (I hope) be a bit of fun.

My plan is to implement the following:

1. Genetic Algorithm framework     **complete**
2. Markov-Chain music generation   **complete**
3. Genetically-evolved melodies    *working*
4. Save to file                    *in progress*

The genetic algorithm framework is implemented by creating two data constructions:

1. `GenePool a`, which contains all the basic information about the gene pool, including the list of the genes themselves as well as pool-wide mutation and crossover rates, as well as the functions implementing those operations.
2. `Gene a`, which is an interesting construction: It contains the data that represents it along with a function that evaluates its fitness. This is to allow the GA to accept user input (in the form of a lambda) as the fitness for a gene.

The program will evolve a pool of ten 4-bar melodies (initialized by a Markov Chain, trained on Mozart, Haydn, and Clementi)

Genetically-evolved melodies are becoming a possibility, but the mutation function is very bad.
I will create a number of different mutations and have the mutation function select between them.

All of the music operations will be done using the excellent Euterpea library (which was developed here at Yale).
