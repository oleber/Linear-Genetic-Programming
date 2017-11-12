import lgp.Action.{Division, IfBigger, Log}
import org.specs2.mutable.Specification

class ActionSpec extends Specification {
  "Division" should {
    "normal execution" in {
      val values: Array[Double] = Array(1, 2, 3)
      val action = Division(0, 1, 2)
      action.evaluate(values)
      values(0) must_=== 2f / 3f
    }

    "positive division by 0" in {
      val values: Array[Double] = Array(1, 2, 0)
      val action = Division(0, 1, 2)
      action.evaluate(values)
      values(0) must_=== 1000000.0
    }

    "negative division by 0" in {
      val values: Array[Double] = Array(1, 2, 0)
      val action = Division(0, 1, 2)
      action.evaluate(values)
      values(0) must_=== 1000000.0
    }
  }

  "IfBigger" should {
    val action = IfBigger(1, 2, Log(0, 3))
    "IfBigger assignTo" in {
      action.assignTo must_=== 0
    }

    "IfBigger assignFrom" in {
      action.assignFrom must_=== List(0, 1, 2, 3)
    }

    "IfBigger false" in {
      val valuesL: List[Double] = List(0, 1, 2, 4)
      val values = valuesL.toArray
      action.evaluate(values)
      valuesL must_=== values.toList
    }

    "IfBigger true" in {
      val valuesL: List[Double] = List(0, 2, 1, 4)
      val values = valuesL.toArray
      action.evaluate(values)
      val expected = valuesL.toArray
      expected(0) = Math.log(4).toFloat
      expected.toList must_=== values.toList
    }
  }
}
