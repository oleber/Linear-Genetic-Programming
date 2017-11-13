package lgp.crossover

import lgp.Model.Problem
import lgp.{Crossover, Model}

import scala.util.Random

class CrossoverRandom(maxSize: Int)(implicit problem: Problem, random: Random) extends Crossover {
  override def crossover(individual1: Model.Individual, individual2: Model.Individual): Model.Individual = {
    val start1 = random.nextInt(individual1.actions.size)
    val count1 = random.nextInt(maxSize)

    val start2 = random.nextInt(individual2.actions.size)
    val count2 = random.nextInt(maxSize)

    crossoverIndividual(
      individual1 = individual1,
      start1 = start1,
      count1 = 1 + count1,
      actions2 = individual2.actions,
      start2 = start2,
      count2 = 1 + count2
    )
  }
}
