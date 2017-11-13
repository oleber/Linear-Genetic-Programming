package lgp

import lgp.Model.Individual

trait Mutation {
  def mutation(individual: Individual): Individual
}
