package com.github.rinotc.scalamath.analysis.sovers

class BrentSolver extends UnivariateSolver {
  def maxEvaluations: Int = ???

  def evaluations: Int = ???

  def absoluteAccuracy: Double = ???

  def relativeAccuracy: Double = ???

  def functionValueAccuracy: Double = ???

  def solve(maxEval: Int, min: Double, max: Double)(f: Double => Double): Either[String, Double] = ???

  def solve(maxEval: Int, min: Double, max: Double, startValue: Double)(f: Double => Double): Either[String, Double] = ???

  def solve(maxEval: Int, startValue: Double)(f: Double => Double): Either[String, Double] = ???
}
