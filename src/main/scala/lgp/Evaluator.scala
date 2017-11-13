package lgp

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}

object Evaluator {
  case class EvaluatedIndividual(individual: Individual, cost: Double)
}

trait Evaluator[SAMPLE] {
  def evaluate(individual: Individual, samples: List[SAMPLE]): EvaluatedIndividual
  def baseline(samples: List[SAMPLE]): Double

  def evaluate(individuals: List[Individual], samples: List[SAMPLE]): List[EvaluatedIndividual] = {
    individuals.map(individual => evaluate(individual, samples))
  }

  def prepareVariables(actions: Vector[Action], problem: Problem): (Individual, Array[Double]) = {
    val constants = Array.fill(problem.inputSize)(false) ++ Array.fill(problem.memorySize)(true)
    val variables = Array.fill(problem.inputSize)(0d) ++ Array.fill(problem.memorySize)(1d)

    @scala.annotation.tailrec
    def step(actions: List[Action], newActions: List[Action]): List[Action] = {
      actions match {
        case action :: tail if (action.assignTo :: action.assignFrom).forall(index => constants(index)) =>
          action.evaluate(variables)
          step(tail, newActions)
        case action :: tail =>
          constants(action.assignTo) = false
          step(tail, action :: newActions)
        case Nil =>
          newActions.reverse
      }
    }

    val newActions = step(actions.toList, Nil)

    (Individual(newActions.toVector, problem), variables.takeRight(problem.memorySize))
  }
}
