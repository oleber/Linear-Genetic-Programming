package lgp.learner

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}
import lgp._

import scala.util.Random

class LearnerTournament[SAMPLE](implicit problem: Problem, random: Random) extends Learner[SAMPLE] {

  override def learn(
                      population: List[Model.Individual],
                      samples: List[SAMPLE],
                      crossovers: Vector[Crossover],
                      evaluator: Evaluator[SAMPLE]
                    ): List[EvaluatedIndividual] = {

    def tournament(
                   participants: List[Individual],
                   samples: List[SAMPLE]
                 )(implicit random: Random): List[EvaluatedIndividual] = {

      if (participants.size != 4) {
        evaluator.evaluate(participants, samples)
      } else {
        val sortedParticipants = evaluator
          .evaluate(participants, samples)
          .sortBy(_.cost)

        val parent1 :: parent2 :: _ = sortedParticipants

        val simplifiedParents: Set[Vector[Action]] = Set(parent1, parent2)
          .map(_.individual.effectiveActions)

        val newIndividuals = evaluator.evaluate(
          createNewIndividuals(parent1.individual, parent2.individual, simplifiedParents, Nil, crossovers, 0),
          samples
        )

        (newIndividuals ++ sortedParticipants).sortBy(_.cost).take(4)
      }
    }

    random.shuffle(population)
      .grouped(4)
      .flatMap(participants => tournament(participants, samples))
      .toList
  }
}
