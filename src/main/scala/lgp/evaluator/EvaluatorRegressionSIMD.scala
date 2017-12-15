package lgp.evaluator

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}
import lgp.Evaluator
import lgp.sample.SampleRegressionSIMD

import scala.util.Random

class EvaluatorRegressionSIMD(implicit problem: Problem, random: Random) extends Evaluator[SampleRegressionSIMD, Array[Array[Double]]] {
  override def createBuffer(samples: SampleRegressionSIMD): Array[Array[Double]] = {
    Array.ofDim[Double](samples.columns.length, samples.columns.head.length)
  }

  override def evaluateSingle(
                               individual: Individual,
                               samples: SampleRegressionSIMD,
                               buffer: Array[Array[Double]]
                             ): EvaluatedIndividual = {
    val SampleRegressionSIMD(columns, expectedValues) = samples

    for ((column, bufferColumn) <- columns zip buffer) {
      Array.copy(column, 0, bufferColumn, 0, column.length)
    }

    individual.evaluateSIMD(buffer)

    val costs = for {
      (expected, result) <- expectedValues zip buffer(0)
      error = expected - result
    } yield error * error

    val sizeFactor = 1 + 0.00001 * individual.effectiveActions.size.toFloat / problem.maxCandidateSize.toFloat

    EvaluatedIndividual(individual, costs.sum * sizeFactor)
  }

  override def baseline(samples: SampleRegressionSIMD): Double = {
    val mean = samples.expectedValues.sum / samples.expectedValues.length

    samples.expectedValues.map(expected => (expected - mean)*(expected - mean)).sum / samples.expectedValues.length
  }
}
