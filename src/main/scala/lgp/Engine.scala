package lgp

import lgp.Learner.EvaluatedIndividuo
import lgp.Model.{Individuo, Problem}

import scala.util.Random

class Engine[SAMPLE](
                      crossovers: Vector[Crossover],
                      mutations: Vector[Mutation],
                      evaluator: Evaluator[SAMPLE],
                      learner: Learner[SAMPLE]
                    ) {
  val startTime = System.currentTimeMillis()

  def learn(
             problem: Problem,
             samples: List[SAMPLE],
             test: List[SAMPLE]
           )(implicit random: Random): List[Individuo] = {
    @scala.annotation.tailrec
    def step(missingSteps: Int, population: List[Individuo]): List[Individuo] = {

      if (missingSteps == 0)
        population
      else {
        val newEvaluatedPopulation = learner.learn(
          individuos = population,
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
          .foreach({ case EvaluatedIndividuo(individuo, cost) =>
            val (resulvedIndeviduo, _) = evaluator.prepareVariables(individuo.efectiveActions, problem)

            val testCost = evaluator.evaluate(individuo, test)
            println(f"${cost / samples.size}%.2f\t${testCost / samples.size}%.2f\t${(testCost / cost - 1) * 100}%.1f%%\t${individuo.actions.size}\t${individuo.efectiveActions.size}\t${resulvedIndeviduo.actions.size}\t${individuo.efectiveActions.reverse}")
            individuo
          })


        step(missingSteps - 1, newEvaluatedPopulation.map(_.individuo))
      }
    }

    val initialPopulation = for {_ <- 1 to problem.numberOfCandidates} yield problem.generateIndividuo

    step(problem.numberOfSteps, initialPopulation.toList)
  }

}
