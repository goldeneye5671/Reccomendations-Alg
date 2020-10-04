package tests

import org.scalatest.FunSuite
import recommendations.Recommendations

class TestStdDev extends FunSuite {

  val EPSILON: Double = 0.01

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test("Test Standard Deviation") {

    val data: List[Double] = List(1.5,3.0,4.5,6.0)

    val stdDev: Double = Recommendations.standardDeviation(data, (d: Double) => d)
    val expected: Double = 2.014944167961

    assert(equalDoubles(stdDev, expected), "\nexpected: " + expected + "\ncomputed: " + stdDev)
  }


}

