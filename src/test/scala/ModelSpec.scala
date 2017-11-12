import lgp.Model.{ActionGenerators, Problem}
import org.specs2.mutable.Specification

class ModelSpec extends Specification {
  "Model" should {
    "Problem" in {
      def problem = Problem(
        outputSize = 1,
        constantsSize = 2,
        inputSize = 3,
        memorySize = 4,
        minCandidateSize = 1,
        maxCandidateSize = 100,
        numberOfCandidates = 1000,
        numberOfSteps = 1000,
        actionGenerators = ActionGenerators(Vector())
      )

      problem.outputIndexes must_=== Vector(0)
      problem.constantsIndexes must_=== Vector(1, 2)
      problem.inputIndexes must_=== Vector(3, 4, 5)
      problem.memoryIndexes must_=== Vector(6, 7, 8, 9)
    }
  }
}
