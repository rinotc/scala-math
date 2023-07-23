package com.github.rinotc.scalastats.common


/**
 * 確率密度関数
 */
trait PDF extends (Double => Double) {
  def apply(x: Double): Double = impl(x).ensuring(_ >= 0, "確率密度関数の値は常に0以上")

  protected def impl(x: Double): Double
}

object PDF {

  def apply(f: Double => Double): PDF = (x: Double) => f(x)
}