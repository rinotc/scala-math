package com.github.rinotc.scalamath.analysis.sovers

import com.github.rinotc.scalamath.BaseFunSpec

import scala.math.*
import BrentSolver._
class BrentSolverTest extends BaseFunSpec {

  describe("solve") {
    describe("sin関数") {
      it("3と4の間で解こうとした時、Piに近い値が得られる") {
        val solver = new BrentSolver(100)(sin)
        val result = solver.solve(3, 4)

        result.value shouldBe Pi +- DefaultAbsoluteAccuracy
        assert(solver.getEvaluationCount <= 7)
      }

      it("1と4の間で解こうとした時、Piに近い値が得られる") {
        val solver = new BrentSolver(100)(sin)
        val result = solver.solve(1, 4)

        result.value shouldBe Pi +- DefaultAbsoluteAccuracy
        assert(solver.getEvaluationCount <= 8)
      }

    }
  }
}
