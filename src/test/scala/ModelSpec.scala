import lgp.Model.{ActionGenerators, Problem}
import org.specs2.mutable.Specification

class ModelSpec extends Specification {
  "Model" should {
    "Problem" in {
      def problem = Problem(
        outputSize = 1,
        memorySize = 4,
        minCandidateSize = 1,
        maxCandidateSize = 100,
        numberOfCandidates = 1000,
        numberOfSteps = 1000,
        actionGenerators = ActionGenerators(Vector())
      )

      problem.outputIndexes must_=== Vector(0)
    }
  }
}
