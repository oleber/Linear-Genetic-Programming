# Linear-Genetic-Programming
Implementation of Linear Genetic Programming.

## Introduction
This project is the result of study and try doing. While it works, it is not ready for production yet.

## Work Done
* Learner
  * Evolution via Tournament, 4 individuals go in and maintains the 2 best. On the other 2 tries to do Crossover or Mutation until find individuals with different Effective Instructions Sets
  * Evolution via Island. This works over other learners. The idea is to divide the full populations in smaller populations. The islands are organized in a ring and slowly let some individuals pass to the next island on the ring. This allow some local searches in different areas of the search space.
* Mutations
  * MutationAddCommand: adds a random instruction
  * MutationDeleteCommand: removes a random instruction
  * MutationRandomPoint: changes an instruction with a random instruction
  * MutationMicro: micro changes to change an existing instruction. For now just the register index is changed, in the future the operation shall also be mutable
* Crossover (if we really can call it like that)
  * CrossoverRandom: a subset of instructions are changed with another individual (usually with a very small size)
  * CrossoverEffectiveRandom: a subset of effective instruction are changed with another individual (usually with a very small size)
  * CrossoverHomogeneous: a subset of instructions are changed with an equal location/size sequence of instruction of another individual
  
  ## Work to Do
  * MutationMicro shall be able to change the instruction leaving the register unchanged
  * Do Unit Tests
  * Track changes on each individual
