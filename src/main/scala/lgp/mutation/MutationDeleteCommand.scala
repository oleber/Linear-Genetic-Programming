package lgp.mutation

import lgp.Model.{Individual, Problem}
import lgp.Mutation

import scala.util.Random

class MutationDeleteCommand(implicit problem: Problem, random: Random) extends Mutation {
  override def mutation(individual: Individual): Individual = {
    if (individual.actions.size < problem.minCandidateSize+1)
      individual
    else {
      val position = random.nextInt(individual.actions.size)
      individual.copy(actions = individual.actions.patch(position, Nil, 1))
    }
  }
}
