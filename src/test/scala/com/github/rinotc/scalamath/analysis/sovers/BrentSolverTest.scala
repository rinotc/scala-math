package com.github.rinotc.scalamath.analysis.sovers

import com.github.rinotc.scalamath.BaseFunSpec

import scala.math.*
import BrentSolver._
class BrentSolverTest extends BaseFunSpec {

  describe("solve") {
    describe("sin関数") {
      it("3と4の間で解こうとした時、7回以下の試行回数でPiに近い値が得られる") {
        val solver = BrentSolver(100)(sin)
        val result = solver.solve(3, 4)

        result.value shouldBe Pi +- DefaultAbsoluteAccuracy
        assert(solver.getEvaluationCount <= 7)
      }

      it("1と4の間で解こうとした時、8回以下の試行回数でPiに近い値が得られる") {
        val solver = new BrentSolver(100)(sin)
        val result = solver.solve(1, 4)

        result.value shouldBe Pi +- DefaultAbsoluteAccuracy
        assert(solver.getEvaluationCount <= 8)
      }
    }

    describe("5次関数") {
      val solver = BrentSolver(100) { x => (x - 1) * (x - 0.5) * x * (x + 0.5) * (x + 1) }
      it("0を中心とした、対称閉区間を処理できるかをテストする") {
        val result = solver.solve(-0.2, 0.2)
        result.value shouldBe 0.0 +- solver.absoluteAccuracy
        assert(solver.getEvaluationCount <= 3)
      }

      it("0付近での極値を含む非対称の閉区間") {
        val result = solver.solve(-0.1, 0.3)
        result.value shouldBe 0.0 +- solver.absoluteAccuracy
        assert(solver.getEvaluationCount <= 7)
      }

      it("0付近で2つの極値を含む閉区間") {
        val result = solver.solve(-0.3, 0.45)
        result.value shouldBe 0d +- solver.absoluteAccuracy
        assert(solver.getEvaluationCount <= 8)
      }

      it("0.5付近での無害な区間。この区間において関数は単調") {
        val result = solver.solve(0.3, 0.7)
        result.value shouldBe 0.5 +- solver.absoluteAccuracy
        assert(solver.getEvaluationCount <= 9)
      }

      it("0.5付近での特異な区間. 極値を一つ含む") {
        val result = solver.solve(0.2, 0.6)
        result.value shouldBe 0.5 +- solver.absoluteAccuracy
        assert(solver.getEvaluationCount <= 11)
      }

      it("0.5付近での特異な区間. 極値を2つ含む") {
        val result = solver.solve(0.05, 0.95)
        result.value shouldBe 0.5 +- solver.absoluteAccuracy
        assert(solver.getEvaluationCount <= 11)
      }

      it("1付近の極値を1つ含む閉区間") {
        val result = solver.solve(0.8, 1.2)
        result.value shouldBe 1.0 +- solver.absoluteAccuracy
        assert(solver.getEvaluationCount <= 11)
      }

      it("1前後を含む区間.") {
        val result = solver.solve(0.85, 1.75)
        result.value shouldBe 1.0 +- solver.absoluteAccuracy
        assert(solver.getEvaluationCount <= 11)
      }

      it("1付近の大きい区間") {
        val result = solver.solve(0.55, 1.45)
        result.value shouldBe 1.0 +- solver.absoluteAccuracy
        assert(solver.getEvaluationCount <= 10)
      }

      it("急に増加する挙動をテストするための1付近のとても大きい区間") {
        val result = solver.solve(0.85, 5)
        result.value shouldBe 1.0 +- solver.absoluteAccuracy
        assert(solver.getEvaluationCount <= 15)
      }

      it("最大試行回数を超えた場合は、Leftを返す") {
        val sol    = BrentSolver(5) { x => (x - 1) * (x - 0.5) * x * (x + 0.5) * (x + 1) }
        val result = sol.solve(0.85, 5)
        assert(result.isLeft)
      }
    }
  }
}
