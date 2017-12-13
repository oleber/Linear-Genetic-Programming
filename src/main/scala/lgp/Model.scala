package lgp

import scala.util.Random

object Model {

  case class Problem(
                      outputSize: Int,
                      memorySize: Int,
                      minCandidateSize: Int,
                      maxCandidateSize: Int,
                      numberOfCandidates: Int,
                      numberOfSteps: Int,
                      actionGenerators: ActionGenerators
                    ) {
    def isValid(individual: Individual): Boolean = {
      val size = individual.actions.size
      size >= minCandidateSize && size <= maxCandidateSize
    }

    val outputIndexes: Vector[Int] = (0 until outputSize).toVector


    def generateIndividual(implicit random: Random): Individual = {
      val individualSize = minCandidateSize + Random.nextInt(maxCandidateSize - minCandidateSize)
      val actions = (1 to individualSize).toVector.map(_ => randomAction)
      Individual(actions, this)
    }

    def actionGeneratorsInput(implicit random: Random): ActionGeneratorsInput = {
      ActionGeneratorsInput(
        to = () => random.nextInt(memorySize),
        nextParam = () => random.nextInt(memorySize),
        action = () => randomAction,
        minValue = -10,
        maxValue = 10
      )
    }

    def randomAction(implicit random: Random): Action = {
      val actionGenerator = actionGenerators.generators(random.nextInt(actionGenerators.generators.size))
      actionGenerator(actionGeneratorsInput)
    }
  }

  case class Individual(actions: Vector[Action], problem: Problem) {
    // the code is ugly but improves performance by ~30%
    @inline def evaluate(registers: Array[Double]): Unit = {
      var i = 0
      while (effectiveActionsArray.length > i) {
        effectiveActionsArray(i).evaluate(registers)
        i = i +1
      }
    }

    val effectiveActions: Vector[Action] = {
      def step(program: List[Action], effectiveVariable: Set[Int]): List[Action] = {
        program match {
          case head :: tail if effectiveVariable.contains(head.assignTo) =>
            head :: step(tail, (effectiveVariable - head.assignTo) ++ head.assignFrom)

          case _ :: tail =>
            step(tail, effectiveVariable)

          case Nil => Nil
        }
      }

      step(
        actions.reverse.toList,
        problem.outputIndexes.toSet
      ).reverse.toVector
    }

    val effectiveActionsArray: Array[Action] = effectiveActions.toArray
  }

  case class ActionGeneratorsInput(
                                    to: () => Int,
                                    nextParam: () => Int,
                                    action: () => Action,
                                    minValue: Double,
                                    maxValue: Double
                                  )

  case class ActionGenerators(generators: Vector[ActionGeneratorsInput => Action])

}
