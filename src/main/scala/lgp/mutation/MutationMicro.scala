package lgp.mutation

import lgp.Model.Problem
import lgp.{Model, Mutation}

import scala.util.Random

class MutationMicro(implicit problem: Problem, random: Random) extends Mutation {
  override def mutation(individuo: Model.Individuo): Model.Individuo = {
    if (individuo.actions.nonEmpty) {
      val mutatingPosition = random.nextInt(individuo.actions.size)
      individuo.copy(
        actions = individuo.actions.patch(
          mutatingPosition,
          Seq(individuo.actions(mutatingPosition).microMutation(problem.actionGeneratorsInput)),
          1
        )
      )
    } else {
      individuo
    }
  }
}
