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

class Engine[SAMPLE <: Sample, BUFFER](
                                        crossovers: Vector[Crossover],
                                        mutations: Vector[Mutation],
                                        evaluator: Evaluator[SAMPLE, BUFFER],
                                        learner: Learner[SAMPLE, BUFFER]
                                      ) {
  val startTime: Long = System.currentTimeMillis()

  val allChanges: Vector[Crossover] = crossovers ++ mutations.map(MutationToCrossover.apply)

  def learn(
             problem: Problem,
             samples1: SAMPLE,
             samples2: SAMPLE,
             test: SAMPLE
           )(implicit random: Random): Individual = {
    def dumpIndividual(bestIndividual: EvaluatedIndividual, evaluatedIndividual: EvaluatedIndividual): Unit = {
      val EvaluatedIndividual(individual, cost) = evaluatedIndividual
      val compare = 100 * (evaluatedIndividual.cost - bestIndividual.cost) / bestIndividual.cost

      println(f"${cost / samples1.size}%.02f\t$compare%.02f%%\t${individual.actions.size}\t${individual.effectiveActions.size}\t${individual.effectiveActions.reverse}")
    }

    @scala.annotation.tailrec
    def step(missingSteps: Int, population: List[Individual], bestIndividual: Individual): Individual = {
      if (missingSteps == 0)
        bestIndividual
      else {
        val newPopulation = learner.learn(
          population = population,
          samples1 = samples1,
          samples2 = samples2,
          crossovers = allChanges,
          evaluator = evaluator
        )

        val informTop = 3

        val newEvaluatedPopulation = (
          for {
            group <- newPopulation.grouped(100).toList.par
            buffer = evaluator.createBuffer(test)
            individual <- group
          } yield evaluator.evaluateSingle(individual, test, buffer)
          ).seq

        val maxEvaluatedIndividual = newEvaluatedPopulation.minBy(_.cost)

        val bestEvaluatedIndividual = evaluator.evaluateSingle(bestIndividual, test, evaluator.createBuffer(test))
        val newBestIndividual = List(bestEvaluatedIndividual, maxEvaluatedIndividual).minBy(_.cost)

        println("-----------------------------------------------------------------------------")
        println(s"Population Size: ${population.length}")

        println(s"${(System.currentTimeMillis - startTime) / (problem.numberOfSteps - missingSteps + 1)} microseconds")

        println(s"samples baseline: ${evaluator.baseline(samples1)}")
        println(s"   test baseline: ${evaluator.baseline(test)}")
        println(missingSteps)

        println("Top:")
        dumpIndividual(bestEvaluatedIndividual, newBestIndividual)

        println("This Generation:")
        newEvaluatedPopulation
          .sortBy(_.cost)
          .take(informTop)
          .foreach(individual => dumpIndividual(bestEvaluatedIndividual, individual))

        step(missingSteps - 1, newPopulation, newBestIndividual.individual)
      }
    }

    val initialPopulation = for {_ <- 1 to problem.numberOfCandidates} yield problem.generateIndividual

    step(problem.numberOfSteps, initialPopulation.toList, problem.generateIndividual)
  }

}
