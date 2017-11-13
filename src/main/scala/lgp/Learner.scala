package lgp

import lgp.Learner.EvaluatedIndividual
import lgp.Model.Individual

object Learner {
  case class EvaluatedIndividual(individual: Individual, cost: Double)
}

trait Learner[SAMPLE] {

  def learn(
             population: List[Individual],
             samples: List[SAMPLE],
             crossovers: Vector[Crossover],
             mutations: Vector[Mutation],
             evaluator: Evaluator[SAMPLE]
           ): (List[EvaluatedIndividual])
}
