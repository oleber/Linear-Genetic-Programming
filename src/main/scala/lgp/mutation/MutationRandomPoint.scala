package lgp.mutation

import lgp.Model.Problem
import lgp.{Model, Mutation}

import scala.util.Random

class MutationRandomPoint(implicit problem: Problem, random: Random) extends Mutation {
  override def mutation(individuo: Model.Individuo): Model.Individuo = {
    if (individuo.actions.nonEmpty) {
      individuo.copy(
        actions = individuo.actions.patch(
          random.nextInt(individuo.actions.size),
          Seq(problem.randomAction),
          1
        )
      )
    } else {
      individuo
    }
  }
}
