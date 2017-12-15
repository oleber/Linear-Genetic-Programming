package lgp.learner

import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}
import lgp._

import scala.util.Random

class LearnerTournament[SAMPLE, BUFFER](implicit problem: Problem, random: Random) extends Learner[SAMPLE, BUFFER] {

  override def learn(
                      population: List[Model.Individual],
                      samples1: SAMPLE,
                      samples2: SAMPLE,
                      crossovers: Vector[Crossover],
                      evaluator: Evaluator[SAMPLE, BUFFER]
                    ): List[Individual] = {

    val buffer = evaluator.createBuffer(samples1)

    def tournament(participants: List[Individual]): List[Individual] = {
      if (participants.size != 4) {
        participants
      } else {
        val sortedParticipants = evaluator
          .evaluate(participants, samples1, buffer)
          .sortBy(_.cost)

        val parent1 :: parent2 :: badOnes = sortedParticipants

        val tryIndividuals = List(
          createNewIndividual(parent1.individual, parent2.individual, crossovers),
          createNewIndividual(parent2.individual, parent1.individual, crossovers)
        ) ++ badOnes.map(_.individual)

        val newIndividuals = evaluator.evaluate(tryIndividuals, samples2, buffer)
          .sortBy(_.cost)
          .take(2)

        (newIndividuals ++ List(parent1, parent2)).map(_.individual)
      }
    }

    random.shuffle(population)
      .grouped(4)
      .flatMap(tournament)
      .toList
  }
}
