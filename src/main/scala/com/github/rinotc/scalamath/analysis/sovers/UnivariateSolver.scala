package com.github.rinotc.scalamath.analysis.sovers

import com.github.rinotc.scalamath.util.Incrementor
import org.apache.commons.math3.optim.MaxEval

import scala.util.{Failure, Success, Try}

/**
 * 一変量実数のルート探索アルゴリズムのインターフェース。 実装は与えられた区間において、1つのゼロだけを探索する。
 */
trait UnivariateSolver {

  import UnivariateSolver._

  private lazy val incrementor = new Incrementor(maxCount = maxEvaluations)

  protected def computeObjectiveValue(point: Double): Double = {
    Try {
      incrementor.unsafeIncrement()
      objectiveFunction(point)
    } match
      case Failure(exception) => throw new TooManyEvaluationsException
      case Success(value)     => value
  }

  /**
   * 目的関数
   */
  protected def objectiveFunction: Double => Double

  /**
   * 目的関数の最大実行回数を取得する
   *
   * @return
   *   関数の最大実行回数
   */
  def maxEvaluations: Int

  /**
   * 目的関数の評価回数を取得する。評価回数は `optimize` メソッドの最後の呼び出しに対応する。 メソッドが呼び出されていない場合は 0 を返す
   *
   * @return
   *   目的回数の評価回数
   */
  def getEvaluationCount: Int = incrementor.getCount

  /**
   * ソルバーの絶対精度を取得する。つまり、&epsilon; がソルバーの絶対精度で、 `v` が `solve` メソッドの一つによって 返される値である場合、関数のルートは区間 (`v` - &epsilon;, `v` +
   * &epsilon;)のどこかに存在するはずである。
   *
   * @return
   *   絶対精度
   */
  def absoluteAccuracy: Double

  /**
   * ソルバーの相対精度を取得する。 相対精度に関する契約はgetAbsoluteAccuracy()と同じですが、絶対誤差ではなく相対誤差を使用します。
   * もし&rho;がソルバーの相対精度であり、vが返された値であるならば、関数の根は区間(v - &rho;v, v + &rho;v)のどこかに存在するはずです。
   *
   * @return
   *   相対精度。
   */
  def relativeAccuracy: Double

  /**
   * ソルバーの関数値精度を取得します。 もしvがソルバーによって返された関数fの値であるならば、契約により、|f(v)|はソルバーに設定された関数値精度以下であるべきです。
   *
   * @return
   *   関数値精度。
   */
  def functionValueAccuracy: Double

  /**
   * 与えられた区間内でゼロの根を求め、startValueで開始します。 ソルバーは、区間が一つのゼロの根を囲むことを要求することがあります。
   * 囲むことを要求するソルバーは、エンドポイントの一方が自体が根である場合でも対処できるべきです。
   *
   * @param min
   *   区間の下限。
   * @param max
   *   区間の上限。
   * @param startValue
   *   使用する開始値。
   * @return
   *   関数がゼロとなる値。
   * @throws IllegalArgumentException
   *   もし引数がソルバーが指定した要件を満たさない場合。
   */
  def solve(min: Double, max: Double, startValue: Double): Either[SolveException, Double]

  /**
   * 与えられた区間でゼロ根を求める．ソルバは，区間が1つのゼロ根を括ることを要求することができる. 括弧付けを必要とするソルバは、端点の1つ自体がルートであるケースを扱うことができるはずである。
   *
   * @param min
   *   区間の下限
   * @param max
   *   区間の上限
   * @return
   *   渡された関数がゼロになる値. 評価回数が最大値を超えた場合、[[Left]] を返す
   * @throws IllegalArgumentException
   *   ソルバーに指定された要件を満たさない引数の場合
   */
  def solve(min: Double, max: Double): Either[SolveException, Double] = {
    solve(min, max, min + 0.5 * (max - min))
  }

  /**
   * startValueの近くでゼロの根を求めます。
   * @param startValue
   *   使用する開始値。
   * @return
   *   関数がゼロとなる値。
   * @throws IllegalArgumentException
   *   もし引数がソルバーが指定した要件を満たさない場合。
   */
  def solve(startValue: Double): Either[SolveException, Double] = {
    solve(Double.NaN, Double.NaN, startValue)
  }
}

object UnivariateSolver {

  /**
   * デフォルトの相対精度
   */
  final val DefaultRelativeAccuracy: Double = 1e-14

  /**
   * デフォルトの関数値精度
   */
  final val DefaultFunctionValueAccuracy: Double = 1e-15

  sealed trait SolveException extends RuntimeException

  final case class TooManyEvaluationsException() extends SolveException

  final case class NoBracketingException() extends SolveException
}
