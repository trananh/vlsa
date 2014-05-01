package edu.arizona.sista.vlsa.math

/** Provides some common similarity metrics & distances.
  *
  * @author trananh
  */
object Metrics {

  /** Compute the KL-divergence of Q from P, denoted DKL(P||Q), which is a measure
    * of the information lost when Q is used to approximate P.
    *
    * NOTE: If the quantity 0 ln(0) appears in the formula, it is interpreted as zero.
    *
    * @param P The true probability distribution.
    * @param Q The approximating probability distribution.
    * @return The KL-divergence of Q from P, denoted as DKL(P||Q).
    */
  def klDivergence(P: Iterable[Double], Q: Iterable[Double]): Double = {
    P.zip(Q).filter(_._1 > 0.0).map{ case (p, q) => p * math.log(p / q) }.sum
  }

  /** Compute precision base on true-positives and false-positives.
    * @param tp True positives.
    * @param fp False positives.
    * @return Precision score.
    */
  def precision(tp: Double, fp: Double): Double = if ((tp + fp) == 0.0) 1.0 else tp / (tp + fp)

  /** Compute recall base on true-positives and false-negatives.
    * @param tp True positives.
    * @param fn False negatives.
    * @return Recall score.
    */
  def recall(tp: Double, fn: Double): Double = if ((tp + fn) == 0.0) 1.0 else tp / (tp + fn)

  /** Compute the F1 (harmonic) score base on precision and recall.
    * @param precision Precision score.
    * @param recall Recall score.
    * @return F1 score.
    */
  def F1(precision: Double, recall: Double): Double = {
    val f1 = 2 * (precision * recall) / (precision + recall)
    if (f1.isNaN) 0.0 else f1
  }

}
