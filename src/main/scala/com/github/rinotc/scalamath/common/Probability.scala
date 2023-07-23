package com.github.rinotc.scalamath.common

import scala.annotation.targetName

/**
 * 確率
 *
 * @param value `0.0 <= value <= 1.0`
 */
final case class Probability(value: Double) extends Ordered[Probability] {

  import Probability._

  require(this >= Min && this <= Max, "Probability must between 0.0 and 1.0")

  def isZero: Boolean = this == Min

  def isOne: Boolean = this == Max

  def plus(that: Probability): Probability = Probability(this.value + that.value)

  def minus(that: Probability): Probability = Probability(this.value - that.value)


  @targetName("plusSymbol")
  def +(that: Probability): Probability = this plus that

  @targetName("minusSymbol")
  def -(that: Probability): Probability = this minus that

  def compare(that: Probability): Int = value.compare(that.value)

  override def toString: String = s"${value * 100}%"
}

object Probability {

  private def Min = Probability(0.0)

  private def Max = Probability(1.0)
}
