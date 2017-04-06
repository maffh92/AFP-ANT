This readme explains the structure of our ant project.

It is created by:
Students:
	Carlos Tome Corti?as - 5794854
	Marinus Oosters      - 3370798
	Martijn Fleuren      - 5666163
	Matthew Swart		 - 5597250


For building the project we recommended that you go to the src direction 
and type in the terminal "stack repl". This should build the project and run it.

test-data <- contains test ants

test
	Spec.hs <- This file is used to test our Quickcheck properties. In the first one we test if the generated code is valid. The second checks if the optimized code is still valid. In the last we check if the final state of the similulor is the same for the optimized code and the original code. 

src/
 Abstraction.hs <- In abstraction we created several funtions such that we could write strategy in a cleaner way.
 Ant.hs <- The Ant.hs consist of functions such that we do not have to write the data types ourself.
 RandomSearch.hs <- 
 Simulator.hs <- This is an interface for the Simulator 
 Strategy.hs <- In this file we implemented our strategy.


Genetic
 Evolve.hs <- module for producing lists of random programs of arbitrary length. The structure will be searched for the best program which is then kept. Standalone module.


Simulator <- This is a copy of the original ant_similator.


Ant
 Arbitrary.hs <- 
 Base.hs <- This file contains the basic instructions that are similar to the original ant instructions. 
 Monad.hs <- In this file we defined the functions for using the MonadFix for writing the instruction.  It contains functions like move, drop, etc. 
 Optimization.hs <- This file contains the functions to optimize the Progam(defined in Monad.hs)


Abritrary
 Base