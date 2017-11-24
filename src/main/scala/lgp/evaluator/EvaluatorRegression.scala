package lgp.evaluator

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}
import lgp.{Evaluator, SampleRegression}

import scala.util.Random

class EvaluatorRegression(implicit problem: Problem, random: Random) extends Evaluator[SampleRegression] {
  override def evaluate(individual: Individual, samples: List[SampleRegression]): EvaluatedIndividual = {
    val (resolvedIndividual, variables) = prepareVariables(individual.efectiveActions, problem)

    val costs = for {
      SampleRegression(parameters, result) <- samples
    } yield {
      val registers = parameters ++ variables
      resolvedIndividual.evaluate(registers)

      val error = result - registers(problem.inputSize)

      error * error
    }

    val sizeFactor = 1 + 0.00001 * individual.efectiveActions.size.toFloat / problem.maxCandidateSize.toFloat

    EvaluatedIndividual(individual, costs.sum * sizeFactor)
  }

  override def baseline(samples: List[SampleRegression]): Double = {
    val mean = samples.map(_.expected).sum / samples.size
    val totalSquareError = samples
      .map(sample => {
        val error = sample.expected - mean
        error * error
      })
      .sum
    totalSquareError / samples.size
  }
}
