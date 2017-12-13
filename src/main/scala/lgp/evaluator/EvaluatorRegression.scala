package lgp.evaluator

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}
import lgp.{Evaluator, SampleRegression}

import scala.util.Random

class EvaluatorRegression(implicit problem: Problem, random: Random) extends Evaluator[SampleRegression] {
  override def evaluate(individual: Individual, samples: List[SampleRegression]): EvaluatedIndividual = {
    val input = Array.ofDim[Double](problem.memorySize)

    val costs = samples map { case SampleRegression(parameters, result) =>
      Array.copy(parameters, 0, input, 0, problem.memorySize)

      individual.evaluate(input)

      val error = result - input(problem.outputIndexes(0))

      error * error
    }

    val sizeFactor = 1 + 0.00001 * individual.effectiveActions.size.toFloat / problem.maxCandidateSize.toFloat

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
