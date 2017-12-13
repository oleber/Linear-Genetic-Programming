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
                      mutations: Vector[Mutation],
                      evaluator: Evaluator[SAMPLE]
                    ): List[EvaluatedIndividual] = {

    val allChanges = crossovers ++ mutations.map(MutationToCrossover.apply)

    def tournament(
                   participants: List[Individual],
                   samples: List[SAMPLE]
                 )(implicit random: Random): List[EvaluatedIndividual] = {

      def createNewIndividual(parent1: Individual, parent2: Individual): Individual = {
        val (p1, p2) = if (random.nextInt(2) == 0) (parent1, parent2) else (parent2, parent1)
        allChanges(random.nextInt(allChanges.size)).crossover(p1, p2)
      }

      @scala.annotation.tailrec
      def createNewIndividuals(
                               parent1: Individual,
                               parent2: Individual,
                               simplifiedParents: Set[Vector[Action]],
                               children: List[Individual],
                               iteration: Int
                             ): List[Individual] = {
        if (children.length == 2) {
          children
        } else if (iteration > 10) {
          children
        } else {
          val newIndividual = createNewIndividual(parent1, parent2)

          val newChildren = if (simplifiedParents.contains(newIndividual.effectiveActions)) {
            children
          } else if (!problem.isValid(newIndividual)) {
            children
          } else {
            newIndividual :: children
          }

          createNewIndividuals(parent1, parent2, simplifiedParents, newChildren, iteration + 1)
        }
      }

      val sortedParticipants = evaluator
        .evaluate(participants, samples)
        .sortBy(_.cost)

      val List(parent1, parent2) = sortedParticipants.take(2)

      val simplifiedParents = Set(parent1, parent2)
        .map(_.individual.effectiveActions.toVector)

      val newIndividuals = evaluator.evaluate(
        createNewIndividuals(parent1.individual, parent2.individual, simplifiedParents, Nil, 0),
        samples
      )

      (newIndividuals ++ sortedParticipants).sortBy(_.cost).take(4)
    }

    random.shuffle(population)
      .grouped(4)
      .toList
      .par
      .flatMap(participants => tournament(participants, samples))
      .seq
      .toList
  }
}
