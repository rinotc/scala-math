package com.github.rinotc.scalastats.frequency

import com.github.rinotc.scalastats.common.Percent

import scala.annotation.targetName

/**
 * 度数
 */
final case class Frequency(value: Long) {
  assert(value >= 0L)

  @targetName("divide")
  def /(other: Frequency): Percent =
    Percent(value.toDouble / other.value.toDouble)

  @targetName("plus")
  def +(other: Frequency): Frequency =
    Frequency(this.value + other.value)
}
