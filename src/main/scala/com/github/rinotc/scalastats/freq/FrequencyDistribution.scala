package com.github.rinotc.scalastats.freq

import com.github.rinotc.scalastats.common.Percent
import org.apache.commons.math3.stat.StatUtils

import scala.collection.immutable.{SortedMap, TreeMap}

/**
 * 度数分布
 */
class FrequencyDistribution[CV <: ClassValue](
    private val freqTable: SortedMap[CV, Frequency]
)(using o: Ordering[CV]) {

  /**
   * @return
   *   平均値
   */
  def mean: Double = sumValue.toDouble / sumOfFrequencies.value.toDouble

  /**
   * @note
   *   いい命名が思いつかない
   * @return
   *   階級値と度数の積の和
   */
  def sumValue: Long = {
    var v = 0L
    freqTable.foreach { case (cv, f) =>
      v += cv.value * f.value
    }
    v
  }

  /**
   * @return
   *   度数の合計を返す
   */
  def sumOfFrequencies: Frequency = Frequency(freqTable.values.map(_.value).sum)

  /**
   * @param classValue
   *   階級値
   * @return
   *   該当の階級値が存在すればその度数を返す
   */
  def getFrequency(classValue: CV): Frequency =
    checkContainsClassValue(classValue)
    freqTable(classValue)

  /**
   * @return
   *   階級値の数を返す
   */
  def getClassValueCount: Int = freqTable.keys.size

  /**
   * @param classValue
   *   階級値
   * @return
   *   相対度数（データ全体の大きさを1としたときの、各階級に属する観測値の個数の全体の中での割合）を返す
   */
  def getRelativeFrequency(classValue: CV): Percent = {
    checkContainsClassValue(classValue)
    getFrequency(classValue) / sumOfFrequencies
  }

  /**
   * @param classValue
   *   階級値
   * @return
   *   累積度数
   */
  def getCumulativeFrequency(classValue: CV): Frequency = {
    checkContainsClassValue(classValue)
    val freq = freqTable.filter((cv, _) => o.lteq(cv, classValue)).values.map(_.value).sum
    Frequency(freq)
  }

  /**
   * @param classValue
   *   階級値
   * @return
   *   累積相対度数
   */
  def getCumulativeRelativeFrequency(classValue: CV): Percent = {
    checkContainsClassValue(classValue)
    val freq = getCumulativeFrequency(classValue)
    freq / sumOfFrequencies
  }

  private def checkContainsClassValue(classValue: CV): Unit =
    if !freqTable.contains(classValue) then throw new NoSuchElementException(s"argument $classValue is not contain.")
}

object FrequencyDistribution {

  def of[CV <: ClassValue](map: Map[CV, Frequency])(using Ordering[CV]) =
    new FrequencyDistribution(freqTable = TreeMap.from(map))
}
