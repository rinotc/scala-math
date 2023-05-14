package com.github.rinotc.scalastats.freq

import com.github.rinotc.scalastats.BaseFunSpec
import com.github.rinotc.scalastats.freq.DefaultClass
import com.github.rinotc.scalastats.freq.DefaultClass.*
import com.github.rinotc.scalastats.freq.Frequency.*
import org.scalatest.funspec.AnyFunSpec

class FrequencyDistributionTest extends BaseFunSpec {

  private val zero2ten       = DC("0点以上10点未満", 5)
  private val ten2twenty     = DC("10点以上20点未満", 15)
  private val twenty2thirty  = DC("20点以上30点未満", 25)
  private val thirty2forty   = DC("30点以上40点未満", 35)
  private val forty2fifty    = DC("40点以上50点未満", 45)
  private val fifty2sixty    = DC("50点以上60点未満", 55)
  private val sixty2seventy  = DC("60点以上70点未満", 65)
  private val seventy2eighty = DC("70点以上80点未満", 75)
  private val eighty2ninety  = DC("80点以上90点未満", 85)
  private val ninety2hundred = DC("90点以上100点以下", 95)
  val m: Map[DefaultClass, Frequency] = Map(
    zero2ten       -> Frequency(12),
    ten2twenty     -> Frequency(10),
    twenty2thirty  -> Frequency(19),
    thirty2forty   -> Frequency(42),
    forty2fifty    -> Frequency(72),
    fifty2sixty    -> Frequency(82),
    sixty2seventy  -> Frequency(54),
    seventy2eighty -> Frequency(38),
    eighty2ninety  -> Frequency(25),
    ninety2hundred -> Frequency(19)
  )

  val fd: FrequencyDistribution[DefaultClass] = FrequencyDistribution.of[DefaultClass](m)

  describe("FrequencyDistribution - 度数分布クラスのテスト") {
    it("getFrequency - 引数の階級の度数を取得する") {
      assert(fd.getFrequency(zero2ten) == Frequency(12))
      assert(fd.getFrequency(ten2twenty) == Frequency(10))
      assert(fd.getFrequency(twenty2thirty) == Frequency(19))
      assert(fd.getFrequency(thirty2forty) == Frequency(42))
      assert(fd.getFrequency(forty2fifty) == Frequency(72))
      assert(fd.getFrequency(fifty2sixty) == Frequency(82))
      assert(fd.getFrequency(sixty2seventy) == Frequency(54))
      assert(fd.getFrequency(seventy2eighty) == Frequency(38))
      assert(fd.getFrequency(eighty2ninety) == Frequency(25))
      assert(fd.getFrequency(ninety2hundred) == Frequency(19))
    }

    it("should getClassValueCount") {}

    it("should getCumulativeFrequency") {}

    it("should sumOfFrequencies") {}

    it("should getRelativeFrequency") {}

    it("should of") {}
  }
}
