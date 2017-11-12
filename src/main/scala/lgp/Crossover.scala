package lgp

import lgp.Model.Individuo

trait Crossover {
  def crossoverIndividuo(
                          individuo1: Individuo,
                          start1: Int,
                          count1: Int,
                          actions2: Vector[Action],
                          start2: Int,
                          count2: Int
                        ): Individuo = {
    val actions = individuo1.actions.patch(
      start1,
      actions2.slice(start2, start2 + count2),
      count1
    )

    Individuo(actions, individuo1.problem)
  }

  def crossoverIndividuo(
                          individuo1: Individuo,
                          start1: Int,
                          count1: Int,
                          individuo2: Individuo,
                          start2: Int,
                          count2: Int
                        ): Individuo = {
    crossoverIndividuo(
      individuo1 = individuo1,
      start1 = start1,
      count1 = count1,
      actions2 = individuo2.actions,
      start2 = start2,
      count2 = count2
    )
  }

  def crossover(individuo1: Individuo, individuo2: Individuo): Individuo
}
