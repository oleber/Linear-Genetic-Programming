package lgp

import lgp.Model.{Individual, Problem}

import scala.util.Random

trait Mutation {

  def createEffectiveAction(individual: Individual)(implicit problem: Problem, random: Random): Action = {
    val effectiveRegisters = individual
      .effectiveActions
      .flatMap(_.assignFrom) ++ problem.outputIndexes

    @scala.annotation.tailrec
    def step: Action = {
      val nextAction: Action = problem.randomAction
      if (effectiveRegisters.contains(nextAction.assignTo))
        nextAction
      else
        step
    }

    step
  }

  def mutation(individual: Individual): Individual
}
