package com.github.rinotc.scalamath.analysis.sovers

import UnivariateSolver.*
import com.github.rinotc.scalamath.analysis.sovers.BrentSolver.DefaultAbsoluteAccuracy
import com.github.rinotc.scalamath.util.Precision

import scala.annotation.tailrec
import scala.math.*
import scala.util.Try

class BrentSolver(
    val maxEvaluations: Int,
    val absoluteAccuracy: Double = DefaultAbsoluteAccuracy,
    val relativeAccuracy: Double = DefaultRelativeAccuracy,
    val functionValueAccuracy: Double = DefaultFunctionValueAccuracy
)(protected val objectiveFunction: Double => Double)
    extends UnivariateSolver {

  def doSolve(min: Double, max: Double, initial: Double): Either[SolveException, Double] =

    require(min <= initial && initial <= max, "endpoints do not specify an interval")

    val yInitial = computeObjectiveValue(initial)
    val yMin     = computeObjectiveValue(min)
    val yMax     = computeObjectiveValue(max)

    // 最初の推測が正しければ、それを返す
    if abs(yInitial) <= functionValueAccuracy then Right(initial)
    // 最初の端点が十分であれば、それを返す
    else if abs(yMin) <= functionValueAccuracy then Right(min)
    //
    else if yInitial * yMin < 0 then brent(min, initial, yMin, yInitial)
    else if abs(yMax) <= functionValueAccuracy then Right(max)
    else if yInitial * yMax < 0 then brent(initial, max, yInitial, yMax)
    else Left(NoBracketingException())
  end doSolve

  /**
   * 与えられた区間内で、ゼロ点を探す。
   *
   * @param lo
   *   探索区間の下界
   * @param hi
   *   探索区間の上界
   * @param fLo
   *   探索区間の下界にある関数値
   * @param fHi
   *   探索区間の上界にある関数値
   * @return
   *   関数がゼロである値
   */
  private def brent(lo: Double, hi: Double, fLo: Double, fHi: Double): Either[TooManyEvaluationsException, Double] = {

    @tailrec
    def loop(a: Double, fa: Double, b: Double, fb: Double, c: Double, fc: Double, d: Double, e: Double): Double = {
      val (newA, newFa, newB, newFb, newC, newFc, newD, newE) = {
        if abs(fc) < abs(fb) then (b, fb, c, fc, b, fb, d, e) else (a, fa, b, fb, c, fc, d, e)
      }

      val tol = 2 * relativeAccuracy * abs(newB) + absoluteAccuracy
      val m   = 0.5 * (newC - newB)

      if (abs(m) <= tol || Precision.equals(newFb, 0)) {
        newB // return
      } else {
        val (nextD, nextE) = if (abs(newE) < tol || abs(newFa) <= abs(newFb)) {
          (m, m) // Force Bisection
        } else {
          val s = newFb / newFa
          var (p, q) = if (newA == newC) {
            (2 * m * s, 1 - s) // // Linear interpolation.
          } else {
            // Inverse quadratic interpolation.
            val q = newFa / newFc
            val r = newFb / newFc
            (s * (2 * m * q * (q - r) - (newB - newA) * (r - 1)), (q - 1) * (r - 1) * (s - 1))
          }
          if p > 0 then q = -q else p = -p

          if p >= 1.5 * m * q - abs(tol * q) || p >= abs(0.5 * s * q) then (m, m)
          else (p / q, newD)
        }

        val nextA  = newB
        val nextFa = newFb
        val nextB  = if (abs(nextD) > tol) newB + nextD else if (m > 0) newB + tol else newB - tol
        val nextFb = computeObjectiveValue(nextB)
        if ((nextFb > 0 && newFc > 0) || (nextFb <= 0 && newFc <= 0)) {
          loop(nextA, nextFa, nextB, nextFb, nextA, nextFa, nextB - nextA, nextB - nextA)
        } else {
          loop(nextA, nextFa, nextB, nextFb, newC, newFc, nextB - nextA, nextB - nextA)
        }
      }
    }

    Try(loop(lo, fLo, hi, fHi, lo, fLo, hi - lo, hi - lo)).toEither match {
      case Left(e)  => Left(TooManyEvaluationsException())
      case Right(v) => Right(v)
    }
  }
}

object BrentSolver {

  def apply(maxEvaluations: Int)(f: Double => Double) = new BrentSolver(maxEvaluations)(f)

  /**
   * デフォルトの絶対精度
   */
  final val DefaultAbsoluteAccuracy: Double = 1e-6
}
