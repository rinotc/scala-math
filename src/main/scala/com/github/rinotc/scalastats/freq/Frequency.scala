package com.github.rinotc.scalastats.freq

import scala.annotation.targetName

/**
 * 度数
 */
final case class Frequency(value: Long) {
  assert(value >= 0L)

  @targetName("divide")
  def /(other: Frequency): Double =
    value.toDouble / other.value.toDouble
}
