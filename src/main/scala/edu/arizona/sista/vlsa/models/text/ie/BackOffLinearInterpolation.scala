package edu.arizona.sista.vlsa.models.text.ie

import edu.arizona.sista.vlsa.experiments.NeighborhoodExperiment
import edu.arizona.sista.vlsa.math.Stats
import edu.arizona.sista.vlsa.models.evaluation.Evaluation
import scala.collection.mutable
import scala.collection.mutable.{HashSet, ListBuffer}

/** An information extraction model that breaks the query tuple into multiple different
  * levels of back-off search.  A different model is used to compute the output at
  * each level before all results are linearly interpolated to produce a single answer
  * for the query tuple.
  *
  * The approach follows from Collins1997.
  *
  * @constructor A deleted interpolation model that operates on an indexed corpus.
  *
  * @author trananh
  */
object BackOffLinearInterpolation {

  /** Process all query tuples using a back-off + linear-interpolation approach. Each query is
    * broken into multiple back-off levels and added to different batches of query-levels.
    * In each level, results (retrieved from running the specified search function) are
    * averaged to yield a single distribution representing each level. Finally, result of
    * each levels are linearly interpolated using the specified parameters to form the
    * final output.
    *
    * Based on Collins1997.
    *
    * @param queries Array of query tuples and their associated POS restrictions.
    * @param model The type of underlying extraction model to use for queries, aka a function that
    *              takes a query tuple and produces a list of (word, score) pairs.
    * @param lambdas Parameters of linear interpolation.
    * @param eval The evaluation labels for score tuning (for DEBUG purposes, defaults to None).
    *
    * @return Distribution over mental states for the query, as list of (word, score) pairs.
    */
  def processQueryTuple(queries: Array[List[(String, Option[Set[String]])]],
                        lambdas: Array[Double],
                        model: List[(String, Option[Set[String]])] => List[(String, Double)],
                        eval: Option[Evaluation] = None): List[(String, Double)] = {
    // Store all results in a map
    val allResults = new Array[mutable.HashMap[String, ListBuffer[Double]]](lambdas.size + 1)
    val resultCounts = new Array[Int](lambdas.size + 1)
    for (i <- 0 until allResults.size) {
      allResults(i) = new mutable.HashMap[String, ListBuffer[Double]]()
      resultCounts(i) = 0
    }

    // Cache query results
    val completedQuery = new HashSet[Set[String]]()

    // For each query, formulate a back-off query for each level
    queries.foreach(query => {
      // Note that we reverse the level for ease of understanding, the first level (index 0)
      // corresponds to the query of the most context.
      for (i <- 1 to query.size) {
        val searchTerms = query.slice(0, i)

        assert(i == searchTerms.size)
        val level = allResults.size - searchTerms.size

        if (!completedQuery.contains(searchTerms.map(_._1).toSet)) {

          // Perform the query
          val distribution = model(searchTerms)

          // Add level result
          if (distribution.size > 0 && !distribution(0)._2.isNaN) {
            distribution.foreach(pair => {
              if (!allResults(level).contains(pair._1)) {
                allResults(level).put(pair._1, new ListBuffer[Double]())
              }
              allResults(level).get(pair._1).get.append(pair._2)
            })
            resultCounts(level) += 1

            if (eval.isDefined) {
              NeighborhoodExperiment.estimateCWSAF1Params(distribution, eval.get)
              println(distribution.sortWith(_._2 > _._2).mkString(", "))
            }
          }

          completedQuery.add(searchTerms.map(_._1).toSet)
        }
      }
    })

    // Helper function to grab the average prob for some mental state at some level
    def getResultProb(k: String, i: Int): Double = {
      if (!allResults(i).contains(k))
        return 0.0
      allResults(i).get(k).get.sum / resultCounts(i)
    }

    // Linear interpolate the different levels
    val allKeys = allResults.map(_.keySet).foldLeft(Set[String]())(_ union _)
    val results = allKeys.toList.map(k => {
      val points = new Array[Double](allResults.size)
      for (i <- 0 until points.size) points(i) = getResultProb(k, i)
      (k, Stats.linearInterpolation(lambdas.toList, points.toList))
    })

    Stats.normalizeScores(results).sortBy(_._1)
  }

}
