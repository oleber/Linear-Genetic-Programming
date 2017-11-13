package lgp.learner

import lgp.Learner.EvaluatedIndividual
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

    def tournament(
                   participants: List[Individual],
                   samples: List[SAMPLE]
                 )(implicit random: Random): List[EvaluatedIndividual] = {

      def creatNewIndividual(parent1: Individual, parent2: Individual): Individual = {
        val (p1, p2) = if (random.nextInt(2) == 0) (parent1, parent2) else (parent2, parent1)
        val crossoverParticipant = crossovers(random.nextInt(crossovers.size)).crossover(p1, p2)
        mutations(random.nextInt(mutations.size)).mutation(crossoverParticipant)
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
          val newIndividual = creatNewIndividual(parent1, parent2)

          val newChildren = if (simplifiedParents.contains(newIndividual.efectiveActions)) {
            children
          } else if (!problem.isValid(newIndividual)) {
            children
          } else {
            newIndividual :: children
          }

          createNewIndividuals(parent1, parent2, simplifiedParents, newChildren, iteration + 1)
        }
      }

      val sortedParticipants = participants
        .map(individual => EvaluatedIndividual(individual, evaluator.evaluate(individual, samples)))
        .sortBy(_.cost)

      val List(parent1, parent2) = sortedParticipants.take(2)

      val simplifiedParent1 = parent1.individual.efectiveActions
      val simplifiedParent2 = parent2.individual.efectiveActions

      val simplifiedParents = Set(simplifiedParent1, simplifiedParent2)

      val newIndividuals = createNewIndividuals(parent1.individual, parent2.individual, simplifiedParents, Nil, 0)
        .map(newIndividual => EvaluatedIndividual(newIndividual, evaluator.evaluate(newIndividual, samples)))

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
