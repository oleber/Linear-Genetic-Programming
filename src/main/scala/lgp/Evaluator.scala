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
}
