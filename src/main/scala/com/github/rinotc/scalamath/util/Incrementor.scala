package com.github.rinotc.scalamath.util

class Incrementor(initial: Int = 0, step: Int = 1, maxCount: Int = Int.MaxValue) {
  require(initial >= 0, "initial count must greater or equal 0.")
  require(step >= 1, "step must be positive.")
  require(maxCount > initial, "maxCount must greater than initial.")

  private var count: Int = initial

  def unsafeIncrement(): Int =
    if count < maxCount then
      count += step
      count
    else throw new Exception("Max count exceeded.")
  end unsafeIncrement

  def increment(): Either[String, Int] =
    if count < maxCount then
      count += step
      Right(count)
    else Left("Max count exceeded.")
  end increment

  def getMaximalCount: Int = maxCount

  def getCount: Int = count

  def canIncrement: Boolean = count < maxCount
}

object Incrementor {

  def create() = new Incrementor()
}
