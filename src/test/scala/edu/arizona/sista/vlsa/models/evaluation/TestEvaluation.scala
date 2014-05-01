package edu.arizona.sista.vlsa.models.evaluation

import edu.arizona.sista.vlsa.models.data.VideoAnnotation
import edu.arizona.sista.vlsa.math.{Stats, Metrics}
import java.io.FileWriter
import junit.framework.Assert._
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/** Tests for different evaluation metrics.
  *
  * @author trananh
  */
class TestEvaluation extends AssertionsForJUnit  {

  val eps = Stats.DoubleEpsilon

  // Create a temp annotation file with distribution: {(angry, 0.9), (afraid, 0.05), (guilty, 0.05)}
  val annotation = new VideoAnnotation("temp.xml", "chase")
  annotation.addMentalStates(VideoAnnotation.Roles.Subject, List.fill(90)("angry") :_*)
  annotation.addMentalStates(VideoAnnotation.Roles.Object, (List.fill(5)("afraid") ++ List.fill(5)("guilty")) :_*)
  val groundTruthDataFile = java.io.File.createTempFile("tmp", ".xml")
  val writer = new FileWriter(groundTruthDataFile)
  writer.write(annotation.toXML())
  writer.close()
  groundTruthDataFile.deleteOnExit()

  // Evaluation instance
  var eval = new Evaluation(groundTruthDataFile.getAbsolutePath)
  eval.log = false

  // Create test sets
  val gold = eval.goldFreq.toSortedListNorm().map(e => (e._1(0), e._2.toDouble)).sortBy(_._1).sortWith(_._2 > _._2)
  val R1 = List(("angry", 0.6), ("mad", 0.3), ("irate", 0.1)).sortWith((_._2 > _._2))
  val R2 = List(("guilty", 0.5), ("scared", 0.5)).sortWith((_._2 > _._2))
  val R3 = List(("sad", 0.85), ("afraid", 0.05), ("angry", 0.05), ("guilty", 0.05)).sortWith((_._2 > _._2))
  val R4 = List(("guilty", 0.7), ("afraid", 0.2), ("angry", 0.1)).sortWith((_._2 > _._2))
  val R5 = List(("irate", 0.45), ("mad", 0.45), ("guilty", 0.05), ("scared", 0.05)).sortWith((_._2 > _._2))

  @Test
  def testKL() {
    // Test R1
    assertEquals(eval.KL(R1.toSet), Double.PositiveInfinity)

    // Test R2
    assertEquals(eval.KL(R2.toSet), Double.PositiveInfinity)

    // Test R3
    val r3 = 0.9 * math.log(0.9 / 0.05) + 0.05 * math.log(0.05 / 0.05) + 0.05 * math.log(0.05 / 0.05)
    assertTrue(math.abs(r3 - eval.KL(R3.toSet)) < eps)

    // Test R4
    val r4 = 0.9 * math.log(0.9 / 0.1) + 0.05 * math.log(0.05 / 0.2) + 0.05 * math.log(0.05 / 0.7)
    assertTrue(math.abs(r4 - eval.KL(R4.toSet)) < eps)

    // Test R5
    assertEquals(eval.KL(R5.toSet), Double.PositiveInfinity)
  }

  @Test
  def testF1() {
    // Test R1
    val (p1, r1) = (1.0 / 3.0, 1.0 / 3.0)
    var f1 = eval.F1(R1.map(_._1).toSet)
    assertTrue(math.abs(p1 - f1._2) < eps)
    assertTrue(math.abs(r1 - f1._3) < eps)
    assertTrue(math.abs(Metrics.F1(p1, r1) - f1._1) < eps)

    // Test R2
    val (p2, r2) = (1.0 / 2.0, 1.0 / 3.0)
    f1 = eval.F1(R2.map(_._1).toSet)
    assertTrue(math.abs(p2 - f1._2) < eps)
    assertTrue(math.abs(r2 - f1._3) < eps)
    assertTrue(math.abs(Metrics.F1(p2, r2) - f1._1) < eps)

    // Test R3
    val (p3, r3) = (3.0 / 4.0, 3.0 / 3.0)
    f1 = eval.F1(R3.map(_._1).toSet)
    assertTrue(math.abs(p3 - f1._2) < eps)
    assertTrue(math.abs(r3 - f1._3) < eps)
    assertTrue(math.abs(Metrics.F1(p3, r3) - f1._1) < eps)

    // Test R4
    val (p4, r4) = (3.0 / 3.0, 3.0 / 3.0)
    f1 = eval.F1(R4.map(_._1).toSet)
    assertTrue(math.abs(p4 - f1._2) < eps)
    assertTrue(math.abs(r4 - f1._3) < eps)
    assertTrue(math.abs(Metrics.F1(p4, r4) - f1._1) < eps)

    // Test R5
    val (p5, r5) = (1.0 / 4.0, 1.0 / 3.0)
    f1 = eval.F1(R5.map(_._1).toSet)
    assertTrue(math.abs(p5 - f1._2) < eps)
    assertTrue(math.abs(r5 - f1._3) < eps)
    assertTrue(math.abs(Metrics.F1(p5, r5) - f1._1) < eps)
  }

  @Test
  def testSAF1() {
    // Test R1
    var scores = Array(Array(1.0, 0.0, 0.0), Array(1.0, 0.0, 0.0), Array(1.0, 0.0, 0.0))
    val (p1, r1) = ((1.0 + 1.0 + 1.0) / 3.0, 1.0 / 3.0)
    println(p1 + " " + r1)
    var f1 = eval.SAF1(R1.map(_._1), gold.map(_._1), scores)
    assertTrue(math.abs(p1 - f1._2) < eps)
    assertTrue(math.abs(r1 - f1._3) < eps)
    println(Metrics.F1(p1, r1))
    assertTrue(math.abs(Metrics.F1(p1, r1) - f1._1) < eps)

    // Test R2
    scores = Array(Array(0.0, 0.0, 1.0), Array(0.0, 1.0, 0.0))
    val (p2, r2) = ((1.0 + 1.0) / 2.0, (1.0 + 1.0) / 3.0)
    println(p2 + " " + r2)
    f1 = eval.SAF1(R2.map(_._1), gold.map(_._1), scores)
    assertTrue(math.abs(p2 - f1._2) < eps)
    assertTrue(math.abs(r2 - f1._3) < eps)
    println(Metrics.F1(p2, r2))
    assertTrue(math.abs(Metrics.F1(p2, r2) - f1._1) < eps)

    // Test R3
    scores = Array(Array(0.0, 0.0, 0.0), Array(0.0, 1.0, 0.0), Array(1.0, 0.0, 0.0), Array(0.0, 0.0, 1.0))
    val (p3, r3) = ((1.0 + 1.0 + 1.0) / 4.0, (1.0 + 1.0 + 1.0) / 3.0)
    println(p3 + " " + r3)
    f1 = eval.SAF1(R3.map(_._1), gold.map(_._1), scores)
    assertTrue(math.abs(p3 - f1._2) < eps)
    assertTrue(math.abs(r3 - f1._3) < eps)
    println(Metrics.F1(p3, r3))
    assertTrue(math.abs(Metrics.F1(p3, r3) - f1._1) < eps)

    // Test R4
    scores = Array(Array(0.0, 0.0, 1.0), Array(0.0, 1.0, 0.0), Array(1.0, 0.0, 0.0))
    val (p4, r4) = ((1.0 + 1.0 + 1.0) / 3.0, (1.0 + 1.0 + 1.0) / 3.0)
    println(p4 + " " + r4)
    f1 = eval.SAF1(R4.map(_._1), gold.map(_._1), scores)
    assertTrue(math.abs(p4 - f1._2) < eps)
    assertTrue(math.abs(r4 - f1._3) < eps)
    println(Metrics.F1(p4, r4))
    assertTrue(math.abs(Metrics.F1(p4, r4) - f1._1) < eps)

    // Test R5
    scores = Array(Array(1.0, 0.0, 0.0), Array(1.0, 0.0, 0.0), Array(0.0, 0.0, 1.0), Array(0.0, 1.0, 0.0))
    val (p5, r5) = ((1.0 + 1.0 + 1.0 + 1.0) / 4.0, (1.0 + 1.0 + 1.0) / 3.0)
    println(p5 + " " + r5)
    f1 = eval.SAF1(R5.map(_._1), gold.map(_._1), scores)
    assertTrue(math.abs(p5 - f1._2) < eps)
    assertTrue(math.abs(r5 - f1._3) < eps)
    println(Metrics.F1(p5, r5))
    assertTrue(math.abs(Metrics.F1(p5, r5) - f1._1) < eps)
  }

  @Test
  def testWSAF1() {
    // Test R1
    var scores = Array(Array(1.0, 0.0, 0.0), Array(1.0, 0.0, 0.0), Array(1.0, 0.0, 0.0))
    val (p1, r1) = (0.6 + 0.3 + 0.1, 0.9)
    println(p1 + " " + r1)
    var f1 = eval.WSAF1(R1, gold, scores, uniformExperiment = false)
    assertTrue(math.abs(p1 - f1._2) < eps)
    assertTrue(math.abs(r1 - f1._3) < eps)
    println(Metrics.F1(p1, r1))
    assertTrue(math.abs(Metrics.F1(p1, r1) - f1._1) < eps)

    // Test R2
    scores = Array(Array(0.0, 0.0, 1.0), Array(0.0, 1.0, 0.0))
    val (p2, r2) = (0.5 + 0.5, 0.05 + 0.05)
    println(p2 + " " + r2)
    f1 = eval.WSAF1(R2, gold, scores, uniformExperiment = false)
    assertTrue(math.abs(p2 - f1._2) < eps)
    assertTrue(math.abs(r2 - f1._3) < eps)
    println(Metrics.F1(p2, r2))
    assertTrue(math.abs(Metrics.F1(p2, r2) - f1._1) < eps)

    // Test R3
    scores = Array(Array(0.0, 0.0, 0.0), Array(0.0, 1.0, 0.0), Array(1.0, 0.0, 0.0), Array(0.0, 0.0, 1.0))
    val (p3, r3) = (0.05 + 0.05 + 0.05, 0.9 + 0.05 + 0.05)
    println(p3 + " " + r3)
    f1 = eval.WSAF1(R3, gold, scores, uniformExperiment = false)
    assertTrue(math.abs(p3 - f1._2) < eps)
    assertTrue(math.abs(r3 - f1._3) < eps)
    println(Metrics.F1(p3, r3))
    assertTrue(math.abs(Metrics.F1(p3, r3) - f1._1) < eps)

    // Test R4
    scores = Array(Array(0.0, 0.0, 1.0), Array(0.0, 1.0, 0.0), Array(1.0, 0.0, 0.0))
    val (p4, r4) = (0.7 + 0.2 + 0.1, 0.9 + 0.05 + 0.05)
    println(p4 + " " + r4)
    f1 = eval.WSAF1(R4, gold, scores, uniformExperiment = false)
    assertTrue(math.abs(p4 - f1._2) < eps)
    assertTrue(math.abs(r4 - f1._3) < eps)
    println(Metrics.F1(p4, r4))
    assertTrue(math.abs(Metrics.F1(p4, r4) - f1._1) < eps)

    // Test R5
    scores = Array(Array(1.0, 0.0, 0.0), Array(1.0, 0.0, 0.0), Array(0.0, 0.0, 1.0), Array(0.0, 1.0, 0.0))
    val (p5, r5) = (0.45 + 0.45 + 0.05 + 0.05, 0.9 + 0.05 + 0.05)
    println(p5 + " " + r5)
    f1 = eval.WSAF1(R5, gold, scores, uniformExperiment = false)
    assertTrue(math.abs(p5 - f1._2) < eps)
    assertTrue(math.abs(r5 - f1._3) < eps)
    println(Metrics.F1(p5, r5))
    assertTrue(math.abs(Metrics.F1(p5, r5) - f1._1) < eps)
  }

  @Test
  def testCWSAF1() {
    // Test R1
    var scores = Array(Array(1.0, 0.0, 0.0), Array(1.0, 0.0, 0.0), Array(1.0, 0.0, 0.0))
    val (p1, r1) = (0.6 + 0.3 + 0.1, 0.9)
    println(p1 + " " + r1)
    var f1 = eval.CWSAF1(R1, gold, scores, uniformExperiment = false)
    assertTrue(math.abs(p1 - f1._2) < eps)
    assertTrue(math.abs(r1 - f1._3) < eps)
    println(Metrics.F1(p1, r1))
    assertTrue(math.abs(Metrics.F1(p1, r1) - f1._1) < eps)

    // Test R2
    scores = Array(Array(0.0, 0.0, 1.0), Array(0.0, 1.0, 0.0))
    val (p2, r2) = (0.05 + 0.05, 0.05 + 0.05)
    println(p2 + " " + r2)
    f1 = eval.CWSAF1(R2, gold, scores, uniformExperiment = false)
    assertTrue(math.abs(p2 - f1._2) < eps)
    assertTrue(math.abs(r2 - f1._3) < eps)
    println(Metrics.F1(p2, r2))
    assertTrue(math.abs(Metrics.F1(p2, r2) - f1._1) < eps)

    // Test R3
    scores = Array(Array(0.0, 0.0, 0.0), Array(0.0, 1.0, 0.0), Array(1.0, 0.0, 0.0), Array(0.0, 0.0, 1.0))
    val (p3, r3) = (0.0 + 0.05 + 0.05 + 0.05, 0.05 + 0.05 + 0.05)
    println(p3 + " " + r3)
    f1 = eval.CWSAF1(R3, gold, scores, uniformExperiment = false)
    assertTrue(math.abs(p3 - f1._2) < eps)
    assertTrue(math.abs(r3 - f1._3) < eps)
    println(Metrics.F1(p3, r3))
    assertTrue(math.abs(Metrics.F1(p3, r3) - f1._1) < eps)

    // Test R4
    scores = Array(Array(0.0, 0.0, 1.0), Array(0.0, 1.0, 0.0), Array(1.0, 0.0, 0.0))
    val (p4, r4) = (0.05 + 0.05 + 0.1, 0.1 + 0.05 + 0.05)
    println(p4 + " " + r4)
    f1 = eval.CWSAF1(R4, gold, scores, uniformExperiment = false)
    assertTrue(math.abs(p4 - f1._2) < eps)
    assertTrue(math.abs(r4 - f1._3) < eps)
    println(Metrics.F1(p4, r4))
    assertTrue(math.abs(Metrics.F1(p4, r4) - f1._1) < eps)

    // Test R5
    scores = Array(Array(1.0, 0.0, 0.0), Array(1.0, 0.0, 0.0), Array(0.0, 0.0, 1.0), Array(0.0, 1.0, 0.0))
    val (p5, r5) = (0.45 + 0.45 + 0.05 + 0.05, 0.9 + 0.05 + 0.05)
    println(p5 + " " + r5)
    f1 = eval.CWSAF1(R5, gold, scores, uniformExperiment = false)
    assertTrue(math.abs(p5 - f1._2) < eps)
    assertTrue(math.abs(r5 - f1._3) < eps)
    println(Metrics.F1(p5, r5))
    assertTrue(math.abs(Metrics.F1(p5, r5) - f1._1) < eps)
  }

}
