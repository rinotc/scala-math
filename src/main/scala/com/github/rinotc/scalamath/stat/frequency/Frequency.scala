package com.github.rinotc.scalamath.stat.frequency

import com.github.rinotc.scalamath.common.Probability

import scala.annotation.targetName

/**
 * 度数
 */
final case class Frequency(value: Long) {
  assert(value >= 0L)

  @targetName("divide")
  def /(other: Frequency): Probability =
    Probability(value.toDouble / other.value.toDouble)

  @targetName("plus")
  def +(other: Frequency): Frequency =
    Frequency(this.value + other.value)
}
