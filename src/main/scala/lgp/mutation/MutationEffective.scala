package lgp.mutation

import lgp.Model.Problem
import lgp.{Action, Model, Mutation}

import scala.util.Random

class MutationEffective(implicit problem: Problem, random: Random) extends Mutation {
  override def mutation(individual: Model.Individual): Model.Individual = {
    val effectiveAction = createEffectiveAction(individual)

    if (individual.actions.isEmpty) {
      individual.copy(actions = Vector(effectiveAction))
    } else {
      val newActions = individual.actions.patch(
        random.nextInt(individual.actions.size),
        Seq(effectiveAction),
        1
      )

      individual.copy(actions = newActions)
    }

  }
}
