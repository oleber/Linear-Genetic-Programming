package lgp.mutation

import lgp.Model.{Individuo, Problem}
import lgp.Mutation

import scala.util.Random

class MutationDeleteCommand(implicit problem: Problem, random: Random) extends Mutation {
  override def mutation(individuo: Individuo): Individuo = {
    if (individuo.actions.size < problem.minCandidateSize+1)
      individuo
    else {
      val position = random.nextInt(individuo.actions.size)
      individuo.copy(actions = individuo.actions.patch(position, Nil, 1))
    }
  }
}
