package lgp.crossover

import lgp.Model.Problem
import lgp.{Crossover, Model}

import scala.util.Random

class CrossoverRandom(maxSize: Int)(implicit problem: Problem, random: Random) extends Crossover {
  override def crossover(individuo1: Model.Individuo, individuo2: Model.Individuo): Model.Individuo = {
    val start1 = random.nextInt(individuo1.actions.size)
    val count1 = random.nextInt(maxSize)

    val start2 = random.nextInt(individuo2.actions.size)
    val count2 = random.nextInt(maxSize)

    crossoverIndividuo(
      individuo1 = individuo1,
      start1 = start1,
      count1 = 1 + count1,
      actions2 = individuo2.actions,
      start2 = start2,
      count2 = 1 + count2
    )
  }
}
