import lgp.Model.{ActionGenerators, Individuo, Problem}
import org.specs2.mutable.Specification

import scala.util.Random

class EngineSpec extends Specification {

  "lgp.Engine" should {
    "xxx" in {
      1 must_=== 2
    }
//    val engine = new lgp.Engine(Nil)
//    "crossover" in {
//      val engine = new lgp.Engine()
//      val ind1 = Individuo(Vector(PI(1), PI(2), PI(3), PI(4), PI(5)))
//      val ind2 = Individuo(Vector(E(1), E(2), E(3), E(4), E(5)))
//
//      engine.crossoverIndividuo(ind1, 1, 3, ind2, 1, 2) must_=== Individuo(Vector(PI(1), E(2), E(3), PI(5)))
//      engine.crossoverIndividuo(ind1, 0, 3, ind2, 1, 2) must_=== Individuo(Vector(E(2), E(3), PI(4), PI(5)))
//      engine.crossoverIndividuo(ind1, 2, 3, ind2, 1, 2) must_=== Individuo(Vector(PI(1), PI(2), E(2), E(3)))
//      engine.crossoverIndividuo(ind1, 1, 3, ind2, 1, 0) must_=== Individuo(Vector(PI(1), PI(5)))
//      engine.crossoverIndividuo(ind1, 1, 3, ind2, 1, 4) must_=== Individuo(Vector(PI(1), E(2), E(3), E(4), E(5), PI(5)))
//    }
//
//    "crossover all Individuos" in {
//      implicit val random: Random = new Random()
//
//      val individuos = for {_ <- 1 to 100 * 1000} yield engine.crossoverRandom(
//        Individuo(Vector(PI(1), PI(2), PI(3), PI(4), PI(5))),
//        Individuo(Vector(E(1), E(2), E(3), E(4), E(5))),
//        Problem(
//          outputSize = 1,
//          constantsSize = 10,
//          inputSize = 10,
//          memorySize = 16,
//          minCandidateSize = 5,
//          maxCandidateSize = 6,
//          numberOfCandidates = 100,
//          numberOfSteps = 1000,
//          actionGenerators = null
//        )
//      )
//
//      individuos.toSet.size must_=== 192
//    }
//
//    def prepareVariablesProblem = Problem(
//      outputSize = 1,
//      constantsSize = 10,
//      inputSize = 4,
//      memorySize = 3,
//      minCandidateSize = 1,
//      maxCandidateSize = 100,
//      numberOfCandidates = 1000,
//      numberOfSteps = 1000,
//      actionGenerators = ActionGenerators(Vector())
//    )
//
//    "prepareVariables single action on constant" in {
//      val participant = Individuo(Vector(Addition(6,4,5)))
//
//      val (newIndeviduo, variables) = engine.prepareVariables(participant, prepareVariablesProblem)
//
//      newIndeviduo must_=== Individuo(Nil.toVector)
//      variables.toList must_=== List(1,1,2)
//    }
//
//    "prepareVariables single action on variable" in {
//      val participant = Individuo(Vector(Addition(6,1,5)))
//
//      val (newIndeviduo, variables) = engine.prepareVariables(participant, prepareVariablesProblem)
//
//      newIndeviduo must_=== Individuo(List(Addition(6,1,5)).toVector)
//      variables.toList must_=== List(1,1,1)
//    }
//
//    "prepareVariables double action on constant" in {
//      val participant = Individuo(Vector(
//        Addition(6,4,5),
//        Multiplication(1, 6, 4)
//      ))
//
//      val (newIndeviduo, variables) = engine.prepareVariables(participant, prepareVariablesProblem)
//
//      newIndeviduo must_=== Individuo(Vector(Multiplication(1, 6, 4)))
//      variables.toList must_=== List(1,1,2)
//    }
  }
}
