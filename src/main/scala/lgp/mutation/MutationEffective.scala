package lgp.mutation

import lgp.Model.Problem
import lgp.{Action, Model, Mutation}

import scala.util.Random

class MutationEffective(implicit problem: Problem, random: Random) extends Mutation {
  override def mutation(individual: Model.Individual) = {
    val effectiveRegisters: Set[Int] = (
      individual.efectiveActions.flatMap(_.assignFrom) ++ problem.outputIndexes
      ).toSet

    @scala.annotation.tailrec
    def effectiveAction: Action = {
      val nextAction: Action = problem.randomAction
      if (effectiveRegisters.contains(nextAction.assignTo))
        nextAction
      else
        effectiveAction
    }

    if (individual.actions.isEmpty) {
      individual.copy(actions = Vector(effectiveAction))
    } else {
      individual.copy(
        actions = individual.actions.patch(
          random.nextInt(individual.actions.size),
          Seq(effectiveAction),
          1
        )
      )
    }

  }
}
