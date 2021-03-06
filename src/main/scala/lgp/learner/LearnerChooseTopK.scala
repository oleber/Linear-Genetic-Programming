package lgp.learner

import lgp._
import lgp.Model.{Individual, Problem}

import scala.util.Random

class LearnerChooseTopK[SAMPLE, BUFFER](keepTopPercentage: Int = 25)
                                       (implicit problem: Problem, random: Random) extends Learner[SAMPLE, BUFFER] {

  override def learn(
                      population: List[Individual],
                      samples1: SAMPLE,
                      samples2: SAMPLE,
                      crossovers: Vector[Crossover],
                      evaluator: Evaluator[SAMPLE, BUFFER]
                    ): List[Individual] = {
    val populationLength = population.length

    def generateChildren(top: Array[Individual], generate: Int): List[Individual] = {
      if (generate < 0) {
        Nil
      } else {
        val parent1 = top(random.nextInt(top.length))
        val parent2 = top(random.nextInt(top.length))

        createNewIndividual(parent1, parent2, crossovers) :: generateChildren(top, generate - 1)
      }
    }

    val buffer = evaluator.createBuffer(samples1)

    val sortedPopulation = evaluator
      .evaluate(population, samples1, buffer)
      .sortBy(_.cost)

    val keepTop = populationLength * keepTopPercentage / 100

    val top = sortedPopulation.take(keepTop).toArray
    val generate = populationLength - keepTop

    val children = generateChildren(top.map(_.individual), generate)

    val remainingIndividuals = children ++ sortedPopulation.takeRight(generate).map(_.individual)
    val evaluatedChildren = evaluator
      .evaluate(remainingIndividuals, samples2, buffer)
      .sortBy(_.cost)
      .take(generate)

    (evaluatedChildren ++ top).map(_.individual)
  }
}
