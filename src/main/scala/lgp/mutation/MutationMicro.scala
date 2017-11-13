package lgp.mutation

import lgp.Model.Problem
import lgp.{Model, Mutation}

import scala.util.Random

class MutationMicro(implicit problem: Problem, random: Random) extends Mutation {
  override def mutation(individual: Model.Individual): Model.Individual = {
    if (individual.actions.nonEmpty) {
      val mutatingPosition = random.nextInt(individual.actions.size)
      individual.copy(
        actions = individual.actions.patch(
          mutatingPosition,
          Seq(individual.actions(mutatingPosition).microMutation(problem.actionGeneratorsInput)),
          1
        )
      )
    } else {
      individual
    }
  }
}
