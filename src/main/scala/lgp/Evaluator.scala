package lgp

import lgp.Model.{Individual, Problem}

trait Evaluator[SAMPLE] {
  def evaluate(individual: Individual, samples: List[SAMPLE]): Double
  def baseline(samples: List[SAMPLE]): Double

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
