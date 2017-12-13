package lgp

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.Individual

trait Learner[SAMPLE] {

  case class MutationToCrossover(mutation: Mutation) extends Crossover {
    override def crossover(individual1: Individual, individual2: Individual): Individual = {
      mutation.mutation(individual1)
    }
  }

  def learn(
             population: List[Individual],
             samples: List[SAMPLE],
             crossovers: Vector[Crossover],
             mutations: Vector[Mutation],
             evaluator: Evaluator[SAMPLE]
           ): (List[EvaluatedIndividual])
}
