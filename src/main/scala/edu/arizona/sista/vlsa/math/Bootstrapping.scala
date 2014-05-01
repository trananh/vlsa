package edu.arizona.sista.vlsa.math

import scala.util.Random

/** Implements Bootstrap Re-sampling for statistical testing.
  *
  * @author trananh
  */
object Bootstrapping {

  /* Initialize random number generator */
  val random = new Random()

  /** Use Bootstrap Re-sampling to figure out whether or not the improvement in scores
    * between two different methods is statistically significant.
    * @param baseline Scores for the baseline method.
    * @param model Scores for the improved method.
    * @param N Number of replicates/samples.
    * @param alpha Significance value.
    * @return True if the averaged improvement over the baseline is statistically
    *         significant (p < alpha).
    */
  def sampleImprovement(baseline: Array[Double], model: Array[Double],
                        N: Int = 10000, alpha: Double = 0.05): Boolean = {
    // Difference in scores between the model and the baseline for each data point
    val diff = model.zip(baseline).map(v => v._1 - v._2)

    // The observed value (i.e., the average improvement)
    val observed = diff.sum / diff.size
    println("Baseline average score: " + (baseline.sum / baseline.size))
    println("Model average score: " + (model.sum / model.size))
    println("Observed improvement: " + observed)

    // Difference between the mean scores of bootstrapped samples (N replicates)
    var samples = new Array[Double](N)

    // Generate N different samples
    for (i <- 0 until N) {
      val bootstrap = diff.map(_ => diff(random.nextInt(diff.size)))
      samples(i) = bootstrap.sum / bootstrap.size
    }

    // Sort the bootstrap samples
    samples = samples.sorted
    println("Bootstrap samples range: [" + samples.head + "," + samples.last + "]")

    // Compute the percentile for the 0 value
    var pindex = 0
    while (pindex < samples.size && samples(pindex) <= 0.0) {
      pindex += 1
    }
    println("Index of 0: " + pindex)
    println("Max percentile of 0: " + ((pindex).toDouble / samples.size))

    // Compute the proportion of samples data that is 0 or less (aka. shows no improvement)
    val p = (pindex).toDouble / samples.size

    // Statistically significant if (p < alpha)
    (p < alpha)
  }

  /** Runs a boostrapping experiment. */
  def main(args: Array[String]) {

    val baseline =
      """0.320221919
        |0.327012706
        |0.343352051
        |0.266330333
        |0.323612573
        |0.315862832
        |0.253584488
        |0.320584839
        |0.33664361
        |0.313336744
        |0.334874189
        |0.332550884
      """.stripMargin.split("\n").map(_.trim()).filterNot(_.isEmpty).map(_.toDouble)

    val model =
      """0.433386623
        |0.496244855
        |0.447076502
        |0.476924507
        |0.432720673
        |0.440886169
        |0.386049675
        |0.518195834
        |0.521639178
        |0.396714867
        |0.468540233
        |0.438951467
      """.stripMargin.split("\n").map(_.trim()).filterNot(_.isEmpty).map(v => v.toDouble)

    val p = 0.01
    val result = sampleImprovement(baseline, model, 10000, p)

    println("Statistically significant (p < " + p + ") = " + result)
  }

}
