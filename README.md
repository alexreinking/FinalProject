FinalProject
============

My CPSC 431 Final Project at Yale.

The idea is to genetically evolve a rules-based system that the user likes. The interactive period can take some time to conduct, but with a bit of dedication could (in theory) be a bit of fun.

My plan is to implement the following:

1) Genetic Algorithm framework     (done)
2) Grammar-based music generation  (in progress)
3) Rule inference from MIDI sample (in progress)
4) Genetically-evolved grammars    (in progrss)

Step one has been accomplished by creating a typeclass Gene that implements `mutate`, `crossover`, and `fitness`.
