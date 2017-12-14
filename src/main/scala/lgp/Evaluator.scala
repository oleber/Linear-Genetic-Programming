package lgp

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}

object Evaluator {
  case class EvaluatedIndividual(individual: Individual, cost: Double)
}

trait Evaluator[SAMPLE, BUFFER] {
  def createBuffer(samples: SAMPLE): BUFFER

  def evaluateSingle(individual: Individual, samples: SAMPLE, buffer: BUFFER): EvaluatedIndividual
  def baseline(samples: SAMPLE): Double

  def evaluate(individuals: List[Individual], samples: SAMPLE, buffer: BUFFER): List[EvaluatedIndividual] = {
    individuals.map(individual => evaluateSingle(individual, samples, buffer))
  }
}
