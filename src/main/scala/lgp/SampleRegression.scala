package lgp

object SampleRegression {
  case class SampleRegressionList(list: List[SampleRegression]) extends Sample {
    override def size: Int = list.size
  }
}

case class SampleRegression(parameters: Array[Double], expected: Double)





