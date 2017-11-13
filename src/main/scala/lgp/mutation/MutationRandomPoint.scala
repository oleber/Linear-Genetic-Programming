package lgp.mutation

import lgp.Model.Problem
import lgp.{Model, Mutation}

import scala.util.Random

class MutationRandomPoint(implicit problem: Problem, random: Random) extends Mutation {
  override def mutation(individual: Model.Individual): Model.Individual = {
    if (individual.actions.nonEmpty) {
      individual.copy(
        actions = individual.actions.patch(
          random.nextInt(individual.actions.size),
          Seq(problem.randomAction),
          1
        )
      )
    } else {
      individual
    }
  }
}
