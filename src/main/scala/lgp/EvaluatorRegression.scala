package lgp

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}

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

      Math.pow(error, 2)
    }

    val sizeFactor = 1 + 0.00001 * individual.efectiveActions.size.toFloat / problem.maxCandidateSize.toFloat

    EvaluatedIndividual(individual, costs.sum * sizeFactor)
  }

  override def baseline(samples: List[SampleRegression]): Double = {
    val mean = samples.map(_.expected).sum / samples.size
    samples.map(sample => Math.pow(sample.expected-mean, 2)).sum / samples.size
  }
}
