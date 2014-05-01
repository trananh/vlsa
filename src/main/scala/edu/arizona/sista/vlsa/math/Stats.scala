package edu.arizona.sista.vlsa.math

/** Provides some common statistics functions.
  *
  * @author trananh
  */
object Stats {

  /** Smallest positive value that when divided by does not yield Infinity */
  val SmallestPositiveValue = 1 / Double.MaxValue

  /** Epsilon for double value comparison */
  val DoubleEpsilon = 1E-7

  /** Log base 2 */
  def log2(x: Double) = scala.math.log(x) / scala.math.log(2)

  /** Builds a new collection by applying a function to all successive sublists of the
    * original list and using the elements of the resulting collections (similar to List.flatMap).
    *
    * @param ls Original list.
    * @param f Function to apply.
    * @tparam A Type of elements.
    * @tparam B Element type of the returned collection.
    * @return New list resulting from applying the given collection-valued function f to successive
    *         sublists of the original list and concatenating the results.
    */
  private def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] = {
    ls match {
      case Nil => Nil
      case sl@(_ :: tail) => f(sl) ::: flatMapSublists(tail)(f)
    }
  }

  /** Construct a list of combinations by treating every element in the list
    * as a unique item.  That is all elements are distinct, even if they
    * share the same value.
    *
    * Example:
    *   - ("aab", 1) => "a" "a" "b"
    *   - ("aab", 2) => "aa" "ab" "ab"
    *   - ("aab", 3) => "aab"
    *
    * Source: http://aperiodic.net/phil/scala/s-99/p26.scala
    *
    * @param n Number of elements per combination.
    * @param ls List of elements.
    * @tparam A Type of the elements.
    *
    * @return List of n-elements combinations.
    */
  def combinations[A](n: Int, ls: List[A]): List[List[A]] = {
    if (n == 0) List(Nil)
    else flatMapSublists(ls) {
      sl =>
        combinations(n - 1, sl.tail) map {
          sl.head :: _
        }
    }
  }

  /** Perform a linear interpolation of an arbitrary list of points using a set of lambda weights.
    * The interpolation is as follows:
    *
    * p = l0*p0 + (1 - l0)l1
    * p = l0*p0 + (1 - l0)(l1*p1 + (1 - l1)p2)
    * p = l0*p0 + (1 - l0)(l1*p1 + (1 - l1)(l2*p2 + (1 - l2)p3))
    * ...
    *
    * @param lambdas List of lambda weights.
    * @param points List of points, there should be one more point than are weights.
    *
    * @return The linear interpolation of the given points.
    */
  def linearInterpolation(lambdas: List[Double], points: List[Double]): Double = {
    assert(lambdas.size + 1 == points.size, "There should be exactly 1 more point than there are weights!")
    val interpolation = new Array[Double](lambdas.size + 1)
    interpolation(lambdas.size) = points(lambdas.size)
    for (i <- (lambdas.size - 1) to 0 by -1) {
      interpolation(i) = lambdas(i) * points(i) + (1 - lambdas(i)) * interpolation(i + 1)
    }
    interpolation(0)
  }

  /** Normalize the scores for a list of (word, score) pairs.
    * @param scores List of (word, score) pairs
    * @return List of (word, score) pairs, normalized by the scores.
    */
  def normalizeScores(scores: List[(String, Double)]): List[(String, Double)] = {
    val sum = scores.map(_._2).sum
    scores.map(e => (e._1, e._2 / sum))
  }

  /** Compute the Shanon Entropy for a probability distribution (using log2).
    * @param probs The probability distribution.
    * @return The Shanon Entropy entropy of the probability distribution.
    */
  def entropy(probs: Iterable[Double]): Double = {
    -1 * probs.filter(_ > 0.0).map(p => p * log2(p)).sum
  }

  /** Compute the Shanon Entropy for a list of (word, frequency) pairs (using log2).
    * @param frequencies The list of (word, frequency) pairs.
    * @return The Shanon Entropy entropy for the list of (word, frequency) pairs.
    */
  def entropy(frequencies: List[(String, Int)]): Double = {
    val norm = normalizeScores(frequencies.map(t => (t._1, t._2.toDouble)))
    entropy(norm.map(_._2))
  }

  /** Compute the mean.
    * @param values List of values.
    * @return The mean.
    */
  def mean(values: Iterable[Double]): Double = values.sum / values.size

  /** Compute the sample variance.
    * @param values List of values.
    * @return The sample variance.
    */
  def variance(values: Iterable[Double]): Double = {
    val vave = mean(values)
    val SS = values.map(v => math.pow((v - vave), 2)).sum
    SS / (values.size - 1)
  }

  /** Compute the population variance.
    * @param values List of values.
    * @return The population variance.
    */
  def pvariance(values: Iterable[Double]): Double = {
    val vave = mean(values)
    val SS = values.map(v => math.pow((v - vave), 2)).sum
    SS / values.size
  }

}
