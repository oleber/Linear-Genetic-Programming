package lgp

import lgp.Learner.EvaluatedIndividual
import lgp.Model.{Individual, Problem}

import scala.util.Random

class Engine[SAMPLE](
                      crossovers: Vector[Crossover],
                      mutations: Vector[Mutation],
                      evaluator: Evaluator[SAMPLE],
                      learner: Learner[SAMPLE]
                    ) {
  val startTime: Long = System.currentTimeMillis()

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
          crossovers = crossovers,
          mutations = mutations,
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
            val (resulvedIndeviduo, _) = evaluator.prepareVariables(individual.efectiveActions, problem)

            val testCost = evaluator.evaluate(individual, test)
            println(f"${cost / samples.size}%.2f\t${testCost / samples.size}%.2f\t${(testCost / cost - 1) * 100}%.1f%%\t${individual.actions.size}\t${individual.efectiveActions.size}\t${resulvedIndeviduo.actions.size}\t${individual.efectiveActions.reverse}")
            individual
          })


        step(missingSteps - 1, newEvaluatedPopulation.map(_.individual))
      }
    }

    val initialPopulation = for {_ <- 1 to problem.numberOfCandidates} yield problem.generateIndividual

    step(problem.numberOfSteps, initialPopulation.toList)
  }

}
