package com.github.rinotc.scalamath.util

import com.github.rinotc.scalamath.BaseFunSpec
import scala.math._
class PrecisionTest extends BaseFunSpec {

  describe("equals") {
    it("許容されたULP内で等しいかどうかのテスト") {
      assertTrue(Precision.equals(0.0, -0.0, 1))
      assertTrue(Precision.equals(1.0, 1 + ulp(1d), 1))
      assertFalse(Precision.equals(1.0, 1 + 2 * ulp(1d), 1))

      val nUp1  = nextAfter(1d, Double.PositiveInfinity)
      val nnUp1 = nextAfter(nUp1, Double.PositiveInfinity)
      assertTrue(Precision.equals(1.0, nUp1, 1))
      assertTrue(Precision.equals(nUp1, nnUp1, 1))
      assertFalse(Precision.equals(1.0, nnUp1, 1))

      assertTrue(Precision.equals(0.0, ulp(0d), 1))
      assertTrue(Precision.equals(0.0, -ulp(0d), 1))

      assertTrue(Precision.equals(153, 153, 1))

      assertTrue(Precision.equals(153.0, 153.00000000000003, 1))
      assertFalse(Precision.equals(153.0, 153.00000000000006, 1))
      assertTrue(Precision.equals(153.0, 152.99999999999997, 1))
      assertFalse(Precision.equals(153, 152.99999999999994, 1))

      assertTrue(Precision.equals(-128.0, -127.99999999999999, 1))
      assertFalse(Precision.equals(-128.0, -127.99999999999997, 1))
      assertTrue(Precision.equals(-128.0, -128.00000000000003, 1))
      assertFalse(Precision.equals(-128.0, -128.00000000000006, 1))

      assertTrue(Precision.equals(Double.PositiveInfinity, Double.PositiveInfinity, 1))
      assertTrue(Precision.equals(Double.MaxValue, Double.MaxValue, 1))

      assertTrue(Precision.equals(Double.NegativeInfinity, Double.NegativeInfinity, 1))
      assertTrue(Precision.equals(-Double.MaxValue, Double.NegativeInfinity, 1))

      assertFalse(Precision.equals(Double.NaN, Double.NaN, 1))
      assertFalse(Precision.equals(Double.NaN, Double.NaN, 0))
      assertFalse(Precision.equals(Double.NaN, 0, 0))
      assertFalse(Precision.equals(Double.NaN, Double.PositiveInfinity, 0))
      assertFalse(Precision.equals(Double.NaN, Double.NegativeInfinity, 0))

      assertFalse(Precision.equals(Double.NegativeInfinity, Double.PositiveInfinity, 100000))
    }
  }
}
