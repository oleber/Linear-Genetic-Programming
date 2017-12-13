package lgp

import lgp.Engine.MutationToCrossover
import lgp.Evaluator.EvaluatedIndividual
import lgp.Model.{Individual, Problem}

import scala.util.Random

object Engine {
  case class MutationToCrossover(mutation: Mutation) extends Crossover {
    override def crossover(individual1: Individual, individual2: Individual): Individual = {
      mutation.mutation(individual1)
    }
  }
}

class Engine[SAMPLE](
                      crossovers: Vector[Crossover],
                      mutations: Vector[Mutation],
                      evaluator: Evaluator[SAMPLE],
                      learner: Learner[SAMPLE]
                    ) {
  val startTime: Long = System.currentTimeMillis()

  val allChanges: Vector[Crossover] = crossovers ++ mutations.map(MutationToCrossover.apply)

  def learn(
             problem: Problem,
             samples: List[SAMPLE],
             test: List[SAMPLE]
           )(implicit random: Random): List[Individual] = {
    @scala.annotation.tailrec
    def step(missingSteps: Int, population: List[Individual]): List[Individual] = {

      if (missingSteps == 0)
        population
      else {
        val newEvaluatedPopulation = learner.learn(
          population = population,
          samples = samples,
          crossovers = allChanges,
          evaluator = evaluator
        )

        val informTop = 3
        println("-----------------------------------------------------------------------------")
        println(s"${(System.currentTimeMillis() - startTime) / (problem.numberOfSteps - missingSteps + 1)} msec")

        println(s"samples baseline: ${evaluator.baseline(samples)}")
        println(s"   test baseline: ${evaluator.baseline(test)}")
        println(missingSteps)

        newEvaluatedPopulation
          .sortBy(_.cost)
          .take(informTop)
          .foreach({ case EvaluatedIndividual(individual, cost) =>
            val testCost = evaluator.evaluate(individual, test)

            println(f"${cost / samples.size}%.2f\t${testCost.cost / samples.size}%.2f\t${(testCost.cost / cost - 1) * 100}%.1f%%\t${individual.actions.size}\t${individual.effectiveActions.size}\t${individual.effectiveActions.reverse}")
            individual
          })

        step(missingSteps - 1, newEvaluatedPopulation.map(_.individual))
      }
    }

    val initialPopulation = for {_ <- 1 to problem.numberOfCandidates} yield problem.generateIndividual

    step(problem.numberOfSteps, initialPopulation.toList)
  }

}
