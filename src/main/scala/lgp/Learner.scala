package lgp

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}

import scala.util.Random

trait Learner[SAMPLE] {

  @scala.annotation.tailrec
  final def createNewIndividuals(
                                  parent1: Individual,
                                  parent2: Individual,
                                  simplifiedParents: Set[Vector[Action]],
                                  children: List[Individual],
                                  crossovers: Vector[Crossover],
                                  iteration: Int
                          )(implicit problem: Problem, random: Random): List[Individual] = {
    def createNewIndividual(parent1: Individual, parent2: Individual, crossovers: Vector[Crossover])
                           (implicit problem: Problem, random: Random): Individual = {
      val crossover = crossovers(random.nextInt(crossovers.size))
      if (random.nextInt(2) == 0)
        crossover.crossover(parent1, parent2)
      else
        crossover.crossover(parent2, parent1)
    }

    if (children.length == 2) {
      children
    } else if (iteration > 10) {
      children
    } else {
      val newIndividual = createNewIndividual(parent1, parent2, crossovers)

      val newChildren = if (simplifiedParents.contains(newIndividual.effectiveActions)) {
        children
      } else if (!problem.isValid(newIndividual)) {
        children
      } else {
        newIndividual :: children
      }

      createNewIndividuals(parent1, parent2, simplifiedParents, newChildren, crossovers, iteration + 1)
    }
  }

  def learn(
             population: List[Individual],
             samples: List[SAMPLE],
             crossovers: Vector[Crossover],
             evaluator: Evaluator[SAMPLE]
           ): (List[EvaluatedIndividual])
}
