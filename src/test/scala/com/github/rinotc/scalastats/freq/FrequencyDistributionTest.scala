package com.github.rinotc.scalastats.freq

import com.github.rinotc.scalastats.BaseFunSpec
import com.github.rinotc.scalastats.common.Percent
import com.github.rinotc.scalastats.freq.Frequency.*

class FrequencyDistributionTest extends BaseFunSpec {

  private val zero2ten       = ClassValue.withClassName("0点以上10点未満", 5)
  private val ten2twenty     = ClassValue.withClassName("10点以上20点未満", 15)
  private val twenty2thirty  = ClassValue.withClassName("20点以上30点未満", 25)
  private val thirty2forty   = ClassValue.withClassName("30点以上40点未満", 35)
  private val forty2fifty    = ClassValue.withClassName("40点以上50点未満", 45)
  private val fifty2sixty    = ClassValue.withClassName("50点以上60点未満", 55)
  private val sixty2seventy  = ClassValue.withClassName("60点以上70点未満", 65)
  private val seventy2eighty = ClassValue.withClassName("70点以上80点未満", 75)
  private val eighty2ninety  = ClassValue.withClassName("80点以上90点未満", 85)
  private val ninety2hundred = ClassValue.withClassName("90点以上100点以下", 95)
  val m: Map[ClassValue.WithClassName, Frequency] = Map(
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

  val fd: FrequencyDistribution[ClassValue.WithClassName] = FrequencyDistribution.of(m)

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

    it("getClassValueCount - 階級値の数を返す") {
      assert(fd.getClassValueCount == 10)
    }

    it("getRelativeFrequency - 相対度数を取得する") {
      fd.getRelativeFrequency(zero2ten).value shouldBe 0.032 +- 0.001
      fd.getRelativeFrequency(ten2twenty).value shouldBe 0.027 +- 0.001
      fd.getRelativeFrequency(twenty2thirty).value shouldBe 0.051 +- 0.001
      fd.getRelativeFrequency(thirty2forty).value shouldBe 0.113 +- 0.001
      fd.getRelativeFrequency(forty2fifty).value shouldBe 0.193 +- 0.001
      fd.getRelativeFrequency(fifty2sixty).value shouldBe 0.220 +- 0.001
      fd.getRelativeFrequency(sixty2seventy).value shouldBe 0.145 +- 0.001
      fd.getRelativeFrequency(seventy2eighty).value shouldBe 0.102 +- 0.001
      fd.getRelativeFrequency(eighty2ninety).value shouldBe 0.067 +- 0.001
      fd.getRelativeFrequency(ninety2hundred).value shouldBe 0.051 +- 0.001
    }

    it("sumOfFrequencies - 度数の合計を返す") {
      assert(fd.sumOfFrequencies.value == 373)
    }

    it("getCumulativeFrequency - 累積度数を返す") {
      fd.getCumulativeFrequency(zero2ten).value shouldBe 12L
      fd.getCumulativeFrequency(ten2twenty).value shouldBe 22L
      fd.getCumulativeFrequency(twenty2thirty).value shouldBe 41L
      fd.getCumulativeFrequency(thirty2forty).value shouldBe 83L
      fd.getCumulativeFrequency(forty2fifty).value shouldBe 155L
      fd.getCumulativeFrequency(fifty2sixty).value shouldBe 237L
      fd.getCumulativeFrequency(sixty2seventy).value shouldBe 291L
      fd.getCumulativeFrequency(seventy2eighty).value shouldBe 329L
      fd.getCumulativeFrequency(eighty2ninety).value shouldBe 354L
      fd.getCumulativeFrequency(ninety2hundred).value shouldBe 373L
    }

    it("getCumulativeRelativeFrequency - 累積相対度数を返す") {
      fd.getCumulativeRelativeFrequency(zero2ten).value shouldBe 0.032 +- 0.001
      fd.getCumulativeRelativeFrequency(ten2twenty).value shouldBe 0.059 +- 0.001
      fd.getCumulativeRelativeFrequency(twenty2thirty).value shouldBe 0.110 +- 0.001
      fd.getCumulativeRelativeFrequency(thirty2forty).value shouldBe 0.223 +- 0.001
      fd.getCumulativeRelativeFrequency(forty2fifty).value shouldBe 0.416 +- 0.001
      fd.getCumulativeRelativeFrequency(fifty2sixty).value shouldBe 0.635 +- 0.001
      fd.getCumulativeRelativeFrequency(sixty2seventy).value shouldBe 0.780 +- 0.001
      fd.getCumulativeRelativeFrequency(seventy2eighty).value shouldBe 0.882 +- 0.001
      fd.getCumulativeRelativeFrequency(eighty2ninety).value shouldBe 0.949 +- 0.001
      fd.getCumulativeRelativeFrequency(ninety2hundred).value shouldBe 1.0
    }

    it("mean - 平均") {
      val map: Map[ClassValue.Default, Frequency] = Map(
        ClassValue(1) -> Frequency(12),
        ClassValue(2) -> Frequency(15),
        ClassValue(3) -> Frequency(25),
        ClassValue(4) -> Frequency(19),
        ClassValue(5) -> Frequency(8)
      )
      val dist: FrequencyDistribution[ClassValue.Default] = FrequencyDistribution.of(map)
      dist.mean shouldBe 2.949 +- 0.001
    }
  }
}
