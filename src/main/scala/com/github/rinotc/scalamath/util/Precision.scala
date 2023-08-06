package com.github.rinotc.scalamath.util
import scala.math._

/**
 * 数値比較のためのユーティリティ
 */
object Precision {

  /**
   * 符号付き倍数を辞書順に並べるためのオフセット。
   */
  private val SGN_MASK = 0x8000000000000000L

  /**
   * Positive zero bits.
   */
  private val POSITIVE_ZERO_DOUBLE_BITS = java.lang.Double.doubleToRawLongBits(+0.0)

  /**
   * Negative zero bits.
   */
  private val NEGATIVE_ZERO_DOUBLE_BITS = java.lang.Double.doubleToRawLongBits(-0.0)

  /**
   * 引数が等しいか、許容される誤差の範囲内であればtrueを返します（範囲は包括的）
   *
   * <p> 2つの浮動小数点等が等しいと見なされるのは、それらの間に `(maxUlps - 1)` (またはそれ以下の)の浮動小数点数がある場合です。 言い換えると、隣接する二つの浮動小数点数は等しいと見なされます。 </p>
   * @param x
   *   1つ目の値
   * @param y
   *   2つ目の値
   * @param maxUlps
   *   `x` と `y` の間の浮動小数点の間の数は `(maxUlps - 1)` です。
   * @return
   *   `x` と `y` の間に `maxUlps` 未満の浮動小数点の数がある場合は、`true`.
   */
  def equals(x: Double, y: Double, maxUlps: Int = 1): Boolean = {
    val xInt: Long = java.lang.Double.doubleToRawLongBits(x)
    val yInt: Long = java.lang.Double.doubleToRawLongBits(y)

    val isEqual =
      if ((xInt ^ yInt) & SGN_MASK) == 0L then abs(xInt - yInt) <= maxUlps
      else
        val (deltaPlus, deltaMinus) =
          if xInt < yInt then (yInt - POSITIVE_ZERO_DOUBLE_BITS, xInt - NEGATIVE_ZERO_DOUBLE_BITS)
          else (xInt - POSITIVE_ZERO_DOUBLE_BITS, yInt - NEGATIVE_ZERO_DOUBLE_BITS)
        if deltaPlus > maxUlps then false else deltaMinus <= (maxUlps - deltaPlus)

    isEqual && !x.isNaN && !y.isNaN
  }
}
