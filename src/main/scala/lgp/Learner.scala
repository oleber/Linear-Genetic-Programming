package lgp

import lgp.Learner.EvaluatedIndividuo
import lgp.Model.Individuo

object Learner {
  case class EvaluatedIndividuo(individuo: Individuo, cost: Double)
}

trait Learner[SAMPLE] {

  def learn(
             individuos: List[Individuo],
             samples: List[SAMPLE],
             crossovers: Vector[Crossover],
             mutations: Vector[Mutation],
             evaluator: Evaluator[SAMPLE]
           ): (List[EvaluatedIndividuo])
}
