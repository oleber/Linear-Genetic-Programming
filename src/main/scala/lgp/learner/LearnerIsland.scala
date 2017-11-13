package lgp.learner

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.Problem
import lgp._

import scala.util.Random

class LearnerIsland[SAMPLE](groupSize: Int, learner: Learner[SAMPLE])(implicit problem: Problem, random: Random) extends Learner[SAMPLE] {
  override def learn(
                      population: List[Model.Individual],
                      samples: List[SAMPLE],
                      crossovers: Vector[Crossover],
                      mutations: Vector[Mutation],
                      evaluator: Evaluator[SAMPLE]
                    ): List[EvaluatedIndividual] = {

    val groups = population
      .grouped(groupSize)
      .toList
      .par
      .map({ subPopulation =>
        learner
          .learn(subPopulation, samples, crossovers, mutations, evaluator)
          .sortBy(_.cost)
      })
      .seq

    val groupsUpdates = for {
      index <- groups.indices
    } yield {
      groups(index).patch(
        groups(index).size - 1,
        List(groups((index + 1) % groups.size)(random.nextInt(groupSize))),
        1
      )
    }

    groupsUpdates.flatten.toList
  }
}
