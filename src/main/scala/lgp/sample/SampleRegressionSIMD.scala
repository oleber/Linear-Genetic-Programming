package lgp.sample

import lgp.Model.Problem
import lgp.Sample

object SampleRegressionSIMD {
  def apply(sampleRegressions: Array[SampleRegression])(implicit problem: Problem): SampleRegressionSIMD = {
    val columns = Array
      .tabulate(sampleRegressions(0).parameters.length, sampleRegressions.length){case (column, sampleIndex) =>
        sampleRegressions(sampleIndex).parameters(column)
      }

    val expectedValues = Array.tabulate(sampleRegressions.length){ index => sampleRegressions(index).expected}

    new SampleRegressionSIMD(columns, expectedValues)
  }
}

case class SampleRegressionSIMD(columns: Array[Array[Double]], expectedValues: Array[Double]) extends Sample {
  override def size: Int = expectedValues.length
}