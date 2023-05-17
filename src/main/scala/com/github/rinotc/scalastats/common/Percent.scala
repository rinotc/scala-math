package com.github.rinotc.scalastats.common

final case class Percent(value: Double) extends Ordered[Percent] {

  import Percent._

  assert(value >= 0.0 && value <= 1.0, "Percent must between 0.0 and 1.0")

  def compare(that: Percent): Int = value.compare(that.value)

  override def toString: String = s"${value * 100}%"
}
