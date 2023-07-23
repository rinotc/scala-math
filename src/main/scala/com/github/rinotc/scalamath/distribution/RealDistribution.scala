package com.github.rinotc.scalamath.distribution

import com.github.rinotc.scalamath.common.{Probability, PDF as IPDF}
import org.apache.commons.math3.analysis.solvers.UnivariateSolver

import scala.math.*

/**
 * 実数範囲の確率分布
 */
trait RealDistribution {

  type PDF <: IPDF

  /**
   * この確率分布に従う確率変数Xに対して、
   * `P(X=x)` を返す。このメソッドは分布の確率質量関数(PMF)を表す
   *
   * @param x PMFが評価される点
   * @return 点xにおける確率質量関数の値
   */
  def probability(x: Double): Probability = Probability(0d)

  def probability(x0: Double, x1: Double): Probability = {
    require(x1 > x0, s"lower endpoint (x0=$x0) must be less than or equal to upper endpoint (x1=$x1)")
    cumulativeProbability(x1) minus cumulativeProbability(x0)
  }


  def density: PDF

  /**
   * 指定された点`x`で評価されたこの分布の確率密度関数PDFを返す。
   * 一般的にPDF（Probability Density Function, 確率密度関数）は
   * CDF（Cumulative Distribution Function, 累積分布関数）の導関数である。
   * 例えば、[[Double.PositiveInfinity]], [[Double.NaN]], または差分商の下限または上限が返される。
   *
   * @param x PDFが評価される点
   * @return 点`x`における確率密度関数の値
   */
  def density(x: Double): Double = density.apply(x)


  /**
   * この分布に従う確率変数 `X` に対して、このメソッドは `P(X <= x)` を返す。
   * 言い換えると、このメソッドはこの分布の累積分布関数（CDF）を表す
   *
   * @param x CDFが評価される点
   * @return この分布に従う確率変数`X`が`x`以下の値をとる確率
   */
  def cumulativeProbability(x: Double): Probability

  /**
   * この分布の分位関数を計算する。この分布に従う確率分布 `X` に対して次の値を返す。
   * 分位関数は累積分布関数の逆関数。分位関数は特定の確率pに対する確率変数`X`の値示す
   *
   * <ul>
   * <li>`inf{x in R | P(X <= x) >= p} for 0 < p <= 1,`</li>
   * <li>`inf{x in R | P(X <= x) > 0 for p = 0}`</li>
   * </ul>
   *
   * @param p 累積確率
   * @return この分布の最小の `p` 分位点。(`p=0`の場合は最大の0分位点
   */
  def inverseCumulativeProbability(p: Probability): Double =
    if p.isZero then supportLowerBound
    else if p.isOne then supportUpperBound
    else
      val mu = numericMean
      val sig = sqrt(numericVariance)
      val chebyshevApplies = !(mu.isInfinity || mu.isNaN || sig.isInfinity || sig.isNaN)

      var lowerBound = supportLowerBound
      var upperBound = supportUpperBound

      if lowerBound.isNegInfinity then
        if chebyshevApplies then lowerBound = mu - sig * sqrt(1.0 - p.value) / p.value
        else
          lowerBound = -1.0
          while (cumulativeProbability(lowerBound) >= p) lowerBound *= 2.0
        end if
      end if

      if upperBound.isPosInfinity then
        if chebyshevApplies then upperBound = mu + sig * sqrt(p.value / 1.0 - p.value)
        else
          upperBound = 1.0
          while (cumulativeProbability(upperBound) < p) upperBound *= 2.0
        end if
      end if

      val solveFunction: Double => Double = (x: Double) => cumulativeProbability(x).minus(p).value

      // TODO

      0.0
    end if
  end inverseCumulativeProbability


  /**
   * @return この分布の平均値を返す. 未定義の場合は[[Double.NaN]]を返す
   */
  def numericMean: Double

  /**
   * @return この分布の分散の値を返す. 未定義の場合は[[Double.NaN]]を返す
   */
  def numericVariance: Double

  /**
   * この分布に従う確率変数が取りうる値の範囲の下限を取得する。
   * このメソッドは [[inverseCumulativeProbability]] に `0` を
   * 渡した値と一致しなければならない。つまり、このメソッドは以下を返す。
   *
   * `inf {x in R | P(X<=x) > 0}.`
   *
   * @return この分布に従う確率変数が取りうる値の範囲の下限。
   */
  def supportLowerBound: Double

  /**
   * この分布に従う確率変数が取りうる値の範囲の上限を取得する。
   * このメソッドは [[inverseCumulativeProbability]] に `1` を渡した値と一致しなければならない.
   * つまり、このメソッドは以下を返す
   *
   * `inf {x in R | {P(X<=x) = 1}.`
   *
   * @return この分布に従う確率変数が取りうる値の上限。
   */
  def supportUpperBound: Double

  /**
   * @return この分布に従う確率変数が取りうる値の範囲が連続的かどうか？
   */
  def isSupportConnected: Boolean

  /**
   * この分布からサンプリングしたランダムな値を返す
   *
   * @return 乱数
   */
  def sample: Double

  /**
   * この分布からサンプリングした乱数のイテレータを返す
   *
   * @param sampleSize 乱数を生成する数
   * @return 乱数のイテレータ
   * @throws IllegalArgumentException 1以上の整数でないとき
   */
  def sample(sampleSize: Int): Iterable[Double]
}
