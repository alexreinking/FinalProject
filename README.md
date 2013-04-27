CPSC 431 Final Project
======================

My CPSC 431 Final Project at Yale University.

The idea is to genetically evolve a rules-based system that the user likes. The interactive period can take some time to conduct, but with a bit of dedication could (I hope) be a bit of fun.

My plan is to implement the following:

1. Genetic Algorithm framework     **revising**
2. Grammar-based music generation  *in progress*
3. Rule inference from MIDI sample *in progress*
4. Genetically-evolved grammars    *in progress*

Step one has been accomplished by creating a typeclass Gene that implements `mutate`, `crossover`, and `fitness` along with functions to advance generations (lists of Genes).
Pains have been taken to ensure that the fitness function is called exactly once per gene per generation.

It now turns out that getting user-supplied fitness is difficult in Haskell. It will require some restucturing of the GA framework.

All of the music operations will be done using the excellent Euterpea library (which was developed here).
