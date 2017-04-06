Students:
	Carlos Tome Corti?as - 5794854
	Marinus Oosters      - 3370798
	Martijn Fleuren      - 5666163
	Matthew Swart		 - 5597250


test-data <- contains test ants

test
 Spec.hs
 Spec/Optimization.hs

src/
 Abstraction.hs <- In abstraction we created several funtion such that we could write strategy in a cleaner way.
 Ant.hs <- The Ant.hs consist of functions such that we do not have to write the data types ourself.
 Genetic.hs <- 
 Simulator.hs <- This is a interface for the Simulator 
 Strategy.hs <- In this file we implemented our strategy.


Genetic
 Evolve.hs <- module for producing lists of random programs of arbitrary length. The structure will be searched for the best program which is then kept. Standalone module.


Simulator <- This is a copy of the original ant_similator.


Ant
 Arbitrary.hs <- 
 Base.hs <- This file contains the basic instructions that are similar to the original ant instructions. 
 Monad.hs <- In this file we defined implementation the function to use  
 Optimization.hs <- This file contains the functions to optimise the Progam(defined in Monad.hs)


Abritrary
 Base