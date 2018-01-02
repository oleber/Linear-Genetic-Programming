package lgp.evaluator

import lgp.Evaluator.EvaluatedIndividual
import lgp.{Action, Evaluator, Model}

class EvaluatorCached[SAMPLE, BUFFER](evaluator: Evaluator[SAMPLE, BUFFER]) extends Evaluator[SAMPLE, BUFFER] {
  override def createBuffer(samples: SAMPLE) = evaluator.createBuffer(samples)

  var cachedEvaluations = Map.empty[SAMPLE, Map[Vector[Action], Double]]

  override def evaluateSingle(individual: Model.Individual, samples: SAMPLE, buffer: BUFFER) = {
    val costOpt = for {
      cache <- cachedEvaluations.get(samples)
      cost <- cache.get(individual.effectiveActions)
    } yield cost

    val cost = costOpt getOrElse {
      val cost = evaluator.evaluateSingle(individual, samples, buffer).cost

      synchronized {
        val sampleCache = cachedEvaluations.getOrElse(samples, Map.empty)

        cachedEvaluations += samples -> (sampleCache + (individual.effectiveActions -> cost))
      }

      cost
    }

    EvaluatedIndividual(individual, cost)
  }

  override def baseline(samples: SAMPLE) = evaluator.baseline(samples)
}
