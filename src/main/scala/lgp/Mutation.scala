package lgp

import lgp.Model.Individuo

trait Mutation {
  def mutation(individuo: Individuo): Individuo
}
