package sangria.util

import Numeric.Implicits._
object MathUtil {

  def zScore[T: Numeric](value: T, sampleSet: Seq[T]): Double = {
    (value.toDouble - mean(sampleSet)) / stdDev(sampleSet)
  }

  def mean[A](set: Seq[A])(implicit num: Numeric[A]): Double = {
    set.sum.toDouble / set.size
  }

  def variance[A](xs: Seq[A])(implicit num: Numeric[A]): Double = {
    val avg = mean(xs)

    xs.map(_.toDouble).map(a => math.pow(a - avg, 2)).sum / xs.size
  }

  def stdDev[A](a: Seq[A])(implicit num: Numeric[A]): Double = {
    math.sqrt(variance(a))
  }
}
