package lgp.mutation

import lgp.Model.Problem
import lgp.{Model, Mutation}

import scala.util.Random

class MutationAddCommand(implicit problem: Problem, random: Random) extends Mutation {
  override def mutation(individual: Model.Individual): Model.Individual = {

    if (problem.maxCandidateSize > individual.actions.size) {
      val nextAction = problem.randomAction
      if (individual.actions.nonEmpty) {
        individual.copy(
          actions = individual.actions.patch(
            random.nextInt(individual.actions.size),
            Seq(nextAction),
            0
          )
        )
      } else {
        individual.copy(actions = Vector(nextAction))
      }
    } else {
      individual
    }
  }
}
