# Amazing ants!

## This readme explains the structure of our ant project.

It is created by:

-	Carlos Tomé Cortiñas - 5794854
-	Marinus Oosters      - 3370798
-	Martijn Fleuren      - 5666163
-	Matthew Swart		     - 5597250

## Building

For building the project we recommend to use `stack` but use `cabal` at your own
risk.

## Folder structure

### Testsuite (folder test)

In order to run the testsuit use `stack test`. May be warned that the test using
the simulator takes considerable time to run.

  + Spec.hs: This file is used to test our Quickcheck properties. In the first
  one we test if the generated code is valid. The second checks if the
  optimized code is still valid. In the last we check if the final state of
  the similulor is the same for the optimized code and the original code.

  + Spec/Optimization.hs: This file contains random generation of optimizations
                          for testing purposes.


### Library (folder src)

  + Abstraction.hs: In this file we define several higher-order combinators and
    a Bool-like language to have a even more abstract DSL. Everything is built
    from the basic combinators in Ant/Monad.hs but. This allows the DSL programs to be
    written in a more fashionable way.

  + Simulator.hs: This is an interface for the original Simulator to be able to 
                  be used for Genetic/Testing purposes.

  + Strategy.hs: In this file we implemented our strategy.


  + Ant
    - Arbitrary.hs: Random generation of sized Ant programs for
                    Genetic/Testing purposes.

    - Base.hs: In this file is the original DSL of instructions.

    - Monad.hs: This file is the meat of the project where we define the AntT monad
                and the basic combinators of our EDSL.

    - Optimization.hs: This file contains the optimization functions that we implemented and
                      everything related to it.

  + Evolve.hs: Module for producing lists of random programs of arbitrary
               length. The structure will be searched for the best program which is then
               kept. Standalone module.
