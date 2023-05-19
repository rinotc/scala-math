package com.github.rinotc.scalastats.freq

/**
 * 階級値
 */
trait ClassValue {
  def value: Long
}

object ClassValue {

  final case class Default(value: Long) extends ClassValue with Ordered[Default] {
    def compare(that: Default): Int = value.compare(that.value)
  }

  def apply(value: Long): Default = Default(value)

  final case class WithClassName(className: String, value: Long) extends ClassValue with Ordered[WithClassName] {
    def compare(that: WithClassName): Int = value.compare(that.value)
  }

  def withClassName(className: String, value: Long): WithClassName = WithClassName(className, value)
}
