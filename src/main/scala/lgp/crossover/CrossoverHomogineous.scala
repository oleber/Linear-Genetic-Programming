package lgp.crossover

import lgp.{Crossover, Model}

import scala.util.Random

class CrossoverHomogineous(implicit random: Random) extends Crossover {
  override def crossover(individuo1: Model.Individuo, individuo2: Model.Individuo): Model.Individuo = {
    val (individuoLeft, individuoRight) = if (random.nextBoolean())
      (individuo1, individuo2)
    else
      (individuo2, individuo1)

    val size = List(individuoLeft.actions.size, individuoRight.actions.size).min

    val start = random.nextInt(size)
    val count = random.nextInt(size - start)

    crossoverIndividuo(individuoLeft, start, count, individuoRight, start, count)
  }
}
