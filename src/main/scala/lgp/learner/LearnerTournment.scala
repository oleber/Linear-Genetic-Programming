package lgp.learner

import lgp.Learner.EvaluatedIndividuo
import lgp.Model.{Individuo, Problem}
import lgp._

import scala.util.Random

class LearnerTournment[SAMPLE](implicit problem: Problem, random: Random) extends Learner[SAMPLE] {

  override def learn(
                      individuos: List[Model.Individuo],
                      samples: List[SAMPLE],
                      crossovers: Vector[Crossover],
                      mutations: Vector[Mutation],
                      evaluator: Evaluator[SAMPLE]
                    ): List[EvaluatedIndividuo] = {

    def tournment(
                   participants: List[Individuo],
                   samples: List[SAMPLE]
                 )(implicit random: Random): List[EvaluatedIndividuo] = {

      def creatNewIndividuo(parent1: Individuo, parent2: Individuo): Individuo = {
        val (p1, p2) = if (random.nextInt(2) == 0) (parent1, parent2) else (parent2, parent1)
        val crossoverParticipant = crossovers(random.nextInt(crossovers.size)).crossover(p1, p2)
        mutations(random.nextInt(mutations.size)).mutation(crossoverParticipant)
      }

      @scala.annotation.tailrec
      def createNewIndividuos(
                               parent1: Individuo,
                               parent2: Individuo,
                               simplifiedParents: Set[Vector[Action]],
                               children: List[Individuo],
                               iteration: Int
                             ): List[Individuo] = {
        if (children.length == 2) {
          children
        } else if (iteration > 10) {
          children
        } else {
          val newIndividuo = creatNewIndividuo(parent1, parent2)

          val newChildren = if (simplifiedParents.contains(newIndividuo.efectiveActions)) {
            children
          } else if (!problem.isValid(newIndividuo)) {
            children
          } else {
            newIndividuo :: children
          }

          createNewIndividuos(parent1, parent2, simplifiedParents, newChildren, iteration + 1)
        }
      }

      val sortedParticipants = participants
        .map(individuo => EvaluatedIndividuo(individuo, evaluator.evaluate(individuo, samples)))
        .sortBy(_.cost)

      val List(parent1, parent2) = sortedParticipants.take(2)

      val simplifiedParent1 = parent1.individuo.efectiveActions
      val simplifiedParent2 = parent2.individuo.efectiveActions

      val simplifiedParents = Set(simplifiedParent1, simplifiedParent2)

      val newIndividuos = createNewIndividuos(parent1.individuo, parent2.individuo, simplifiedParents, Nil, 0)
        .map(newIndividuo => EvaluatedIndividuo(newIndividuo, evaluator.evaluate(newIndividuo, samples)))

      (newIndividuos ++ sortedParticipants).sortBy(_.cost).take(4)
    }

    random.shuffle(individuos)
      .grouped(4)
      .toList
      .par
      .flatMap(participants => tournment(participants, samples))
      .seq
      .toList
  }
}
