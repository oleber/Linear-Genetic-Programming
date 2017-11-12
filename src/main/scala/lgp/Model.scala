package lgp

import scala.util.Random

object Model {

  case class Problem(
                      outputSize: Int,
                      constantsSize: Int,
                      inputSize: Int,
                      memorySize: Int,
                      minCandidateSize: Int,
                      maxCandidateSize: Int,
                      numberOfCandidates: Int,
                      numberOfSteps: Int,
                      actionGenerators: ActionGenerators
                    ) {
    def isValid(individuo: Individuo): Boolean = {
      val size = individuo.actions.size
      size >= minCandidateSize && size <= maxCandidateSize
    }

    val outputIndexes: Vector[Int] = Vector(inputSize)

    val constantsIndexes: Vector[Int] = (outputSize until outputSize + constantsSize).toVector

    val inputIndexes: Vector[Int] = {
      val start = outputSize + constantsSize
      (start until start + inputSize).toVector
    }

    val memoryIndexes: Vector[Int] = {
      val start = outputSize + constantsSize + inputSize
      (start until start + memorySize).toVector
    }

    def generateIndividuo(implicit random: Random): Individuo = {
      val individuoSize = minCandidateSize + Random.nextInt(maxCandidateSize - minCandidateSize)
      val actions = (1 to individuoSize).toVector.map(_ => randomAction)
      Individuo(actions, this)
    }

    def actionGeneratorsInput(implicit random: Random): ActionGeneratorsInput = {
      ActionGeneratorsInput(
        to = () => random.nextInt(memorySize) + inputSize,
        nextParam = () => random.nextInt(memorySize + inputSize),
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

  case class Individuo(actions: Vector[Action], problem: Problem) {
    def evaluate(registers: Array[Double]): Unit = {
      actions foreach {
        _.evaluate(registers)
      }
    }

    val efectiveActions: Vector[Action] = {
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
