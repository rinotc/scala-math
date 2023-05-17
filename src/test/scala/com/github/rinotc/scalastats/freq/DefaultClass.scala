package com.github.rinotc.scalastats.freq

/**
 * 標準的な階級、階級値
 * @param className
 *   階級
 * @param classValue
 *   階級値
 */
final case class DefaultClass(className: String, classValue: Int) extends Ordered[DefaultClass] with Continuous {
  def compare(that: DefaultClass): Int = classValue.compare(that.classValue)
}

object DefaultClass {

  def DC(className: String, classValue: Int): DefaultClass =
    apply(className, classValue)
}
