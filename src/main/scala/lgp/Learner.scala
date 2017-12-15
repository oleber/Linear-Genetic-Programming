package lgp

import lgp.Model.{Individual, Problem}
import scala.util.Random

trait Learner[SAMPLE, BUFFER] {

  def createNewIndividual(
                           parent1: Individual,
                           parent2: Individual,
                           crossovers: Vector[Crossover]
                         )(implicit problem: Problem, random: Random): Individual = {
    val simplifiedParents = Set(parent1.effectiveActions, parent2.effectiveActions)

    def create(): Individual = {
      val crossover = crossovers(random.nextInt(crossovers.size))
      val newIndividual = if (random.nextInt(2) == 0)
        crossover.crossover(parent1, parent2)
      else
        crossover.crossover(parent2, parent1)

      newIndividual
    }

    def step(iteration: Int): Individual = {
      val newIndividual = create()
      if (iteration > 10) {
        newIndividual
      } else if (simplifiedParents.contains(newIndividual.effectiveActions)) {
        step(iteration + 1)
      } else {
        newIndividual
      }
    }

    step(0)
  }

  def learn(
             population: List[Individual],
             samples1: SAMPLE,
             samples2: SAMPLE,
             crossovers: Vector[Crossover],
             evaluator: Evaluator[SAMPLE, BUFFER]
           ): List[Individual]
}
