package com.github.rinotc.scalastats.freq

import com.github.rinotc.scalastats.common.Percent

import scala.collection.immutable.{SortedMap, TreeMap}

/**
 * 度数分布
 */
final class FrequencyDistribution[K](
    private val freqTable: SortedMap[K, Frequency]
)(using o: Ordering[K]) {

  /**
   * @return
   *   頻度の合計を返す
   */
  def sumOfFrequencies: Frequency = Frequency(freqTable.values.map(_.value).sum)

  /**
   * @param classValue
   *   階級値
   * @return
   *   該当の階級値が存在すればその度数を返す
   */
  def getFrequency(classValue: K): Frequency =
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
  def getRelativeFrequency(classValue: K): Percent = {
    checkContainsClassValue(classValue)
    getFrequency(classValue) / sumOfFrequencies
  }

  /**
   * @param classValue
   *   階級値
   * @return
   *   累積度数
   */
  def getCumulativeFrequency(classValue: K): Frequency = {
    checkContainsClassValue(classValue)
    val freq = freqTable.filter((cv, _) => o.lteq(cv, classValue)).values.map(_.value).sum
    Frequency(freq)
  }

  def getCumulativeRelativeFrequency(classValue: K): Percent = {
    checkContainsClassValue(classValue)
    val freq = getCumulativeFrequency(classValue)
    freq / sumOfFrequencies
  }

  private def checkContainsClassValue(classValue: K): Unit =
    if !freqTable.contains(classValue) then throw new NoSuchElementException(s"argument $classValue is not contain.")
}

object FrequencyDistribution {

  def of[K: Ordering](map: Map[K, Frequency]) =
    new FrequencyDistribution(freqTable = TreeMap.from(map))
}
