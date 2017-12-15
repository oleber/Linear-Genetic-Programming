package lgp.learner

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}
import lgp._

import scala.util.Random

class LearnerTournament[SAMPLE, BUFFER](implicit problem: Problem, random: Random) extends Learner[SAMPLE, BUFFER] {

  override def learn(
                      population: List[Model.Individual],
                      samples: SAMPLE,
                      crossovers: Vector[Crossover],
                      evaluator: Evaluator[SAMPLE, BUFFER]
                    ): List[EvaluatedIndividual] = {

    val buffer = evaluator.createBuffer(samples)

    def tournament(participants: List[Individual], samples: SAMPLE): List[EvaluatedIndividual] = {
      if (participants.size != 4) {
        evaluator.evaluate(participants, samples, buffer)
      } else {
        val sortedParticipants = evaluator
          .evaluate(participants, samples, buffer)
          .sortBy(_.cost)

        val parent1 :: parent2 :: _ = sortedParticipants

        val simplifiedParents: Set[Vector[Action]] = Set(parent1, parent2)
          .map(_.individual.effectiveActions)

        val newIndividuals = evaluator.evaluate(
          List(
            createNewIndividual(parent1.individual, parent2.individual, crossovers),
            createNewIndividual(parent2.individual, parent1.individual, crossovers)
          ),
          samples,
          buffer
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
