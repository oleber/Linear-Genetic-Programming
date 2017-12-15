package lgp.sample

import lgp.Model.Problem
import lgp.Sample

object SampleRegressionSIMD {
  def apply(sampleRegressions: Array[SampleRegression])(implicit problem: Problem): SampleRegressionSIMD = {
    val columns = Array
      .tabulate(sampleRegressions(0).parameters.size, sampleRegressions.size){case (column, sampleIndex) =>
        sampleRegressions(sampleIndex).parameters(column)
      }

    val expecteds = Array.tabulate(sampleRegressions.size){index => sampleRegressions(index).expected}

    new SampleRegressionSIMD(columns, expecteds)
  }
}

case class SampleRegressionSIMD(columns: Array[Array[Double]], expecteds: Array[Double]) extends Sample {
  override def size: Int = expecteds.size
}