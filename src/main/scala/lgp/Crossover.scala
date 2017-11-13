package lgp

import lgp.Model.Individual

trait Crossover {
  def crossoverIndividual(
                          individual1: Individual,
                          start1: Int,
                          count1: Int,
                          actions2: Vector[Action],
                          start2: Int,
                          count2: Int
                        ): Individual = {
    val actions = individual1.actions.patch(
      start1,
      actions2.slice(start2, start2 + count2),
      count1
    )

    Individual(actions, individual1.problem)
  }

  def crossoverIndividual(
                          individual1: Individual,
                          start1: Int,
                          count1: Int,
                          individual2: Individual,
                          start2: Int,
                          count2: Int
                        ): Individual = {
    crossoverIndividual(
      individual1 = individual1,
      start1 = start1,
      count1 = count1,
      actions2 = individual2.actions,
      start2 = start2,
      count2 = count2
    )
  }

  def crossover(individual1: Individual, individual2: Individual): Individual
}
