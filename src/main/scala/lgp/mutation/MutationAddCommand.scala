package lgp.mutation

import lgp.Model.Problem
import lgp.{Model, Mutation}

import scala.util.Random

class MutationAddCommand(implicit problem: Problem, random: Random) extends Mutation {
  override def mutation(individuo: Model.Individuo): Model.Individuo = {

    if (problem.maxCandidateSize > individuo.actions.size) {
      val nextAction = problem.randomAction
      if (individuo.actions.nonEmpty) {
        individuo.copy(
          actions = individuo.actions.patch(
            random.nextInt(individuo.actions.size),
            Seq(nextAction),
            0
          )
        )
      } else {
        individuo.copy(actions = Vector(nextAction))
      }
    } else {
      individuo
    }
  }
}
