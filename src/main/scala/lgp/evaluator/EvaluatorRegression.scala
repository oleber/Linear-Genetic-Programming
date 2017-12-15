package lgp.evaluator

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}
import lgp.Evaluator
import lgp.sample.SampleRegression
import lgp.sample.SampleRegression.SampleRegressionList

import scala.util.Random

class EvaluatorRegression(implicit problem: Problem, random: Random)
  extends Evaluator[SampleRegressionList, Array[Double]] {

  override def createBuffer(samples: SampleRegressionList): Array[Double] = {
    Array.ofDim[Double](problem.memorySize)
  }

  override def evaluateSingle(
                         individual: Individual,
                         samples: SampleRegressionList,
                         buffer: Array[Double]
                       ): EvaluatedIndividual = {
    val costs = samples.list map { case SampleRegression(parameters, result) =>
      Array.copy(parameters, 0, buffer, 0, problem.memorySize)

      individual.evaluate(buffer)

      val error = result - buffer(problem.outputIndexes(0))

      error * error
    }

    val cost = individual.effectiveActions.map(_.cost).sum
    val sizeFactor = 1 + 0.00001 * cost / problem.maxCandidateSize.toFloat

    EvaluatedIndividual(individual, costs.sum * sizeFactor)
  }

  override def baseline(samples: SampleRegressionList): Double = {
    val mean = samples.list.map(_.expected).sum / samples.size
    val totalSquareError = samples.list
      .map(sample => {
        val error = sample.expected - mean
        error * error
      })
      .sum
    totalSquareError / samples.size
  }
}
