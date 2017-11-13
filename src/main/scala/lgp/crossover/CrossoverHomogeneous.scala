package lgp.crossover

import lgp.{Crossover, Model}

import scala.util.Random

class CrossoverHomogeneous(implicit random: Random) extends Crossover {
  override def crossover(individual1: Model.Individual, individual2: Model.Individual): Model.Individual = {
    val (individualLeft, individualRight) = if (random.nextBoolean())
      (individual1, individual2)
    else
      (individual2, individual1)

    val size = List(individualLeft.actions.size, individualRight.actions.size).min

    val start = random.nextInt(size)
    val count = random.nextInt(size - start)

    crossoverIndividual(individualLeft, start, count, individualRight, start, count)
  }
}
