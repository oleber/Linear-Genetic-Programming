package lgp.learner

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.Problem
import lgp._

import scala.util.Random

class LearnerIsland[SAMPLE, BUFFER](groupSize: Int, learner: Learner[SAMPLE, BUFFER])
                                   (implicit problem: Problem, random: Random) extends Learner[SAMPLE, BUFFER] {
  override def learn(
                      population: List[Model.Individual],
                      samples: SAMPLE,
                      crossovers: Vector[Crossover],
                      evaluator: Evaluator[SAMPLE, BUFFER]
                    ): List[EvaluatedIndividual] = {

    val groups = population
      .grouped(groupSize)
      .toList
      .par
      .map({ subPopulation =>
        learner
          .learn(subPopulation, samples, crossovers, evaluator)
          .sortBy(_.cost)
      })
      .seq

    val groupsUpdates = for {
      index <- groups.indices
      patch = List(groups((index + 1) % groups.size)(random.nextInt(groupSize)))
    } yield {
      groups(index).patch(groups(index).size - 1, patch, 1)
    }

    groupsUpdates.flatten.toList
  }
}
