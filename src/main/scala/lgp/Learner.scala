package lgp

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.Individual

trait Learner[SAMPLE] {

  def learn(
             population: List[Individual],
             samples: List[SAMPLE],
             crossovers: Vector[Crossover],
             mutations: Vector[Mutation],
             evaluator: Evaluator[SAMPLE]
           ): (List[EvaluatedIndividual])
}
