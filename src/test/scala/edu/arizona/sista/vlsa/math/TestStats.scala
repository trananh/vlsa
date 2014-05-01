package edu.arizona.sista.vlsa.math

import junit.framework.Assert._
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/** Tests for Stats.
  *
  * @author trananh
  */
class TestStats extends AssertionsForJUnit {

  @Test
  def testCombinations() {
    // Test with non-repeating values
    var l = List("1", "2", "3", "4", "5")
    for (i <- 1 to l.size) {
      assertEquals(Stats.combinations(i, l).toList.mkString(" "), l.combinations(i).mkString(" "))
    }

    // Test with repeating values that should be treated as distinct items
    l = List("1", "1", "2", "2", "3", "3", "3")
    println(Stats.combinations(3, l).toList.mkString(" "))
    val unique = l.zipWithIndex.map(valueIdx => valueIdx._1 + valueIdx._2.toString)
    for (i <- 1 to l.size) {
      assertEquals(Stats.combinations(i, l).toList.mkString(" "),
        unique.combinations(i).map(combos => combos.map(value => value.dropRight(1))).mkString(" "))
    }
  }


  @Test
  def testLinearInterpolation() {
    val lambdas = List(0.2, 0.6, 0.4)
    val points = List(0.0, 4., 9., 4.)

    val r1 = Stats.linearInterpolation(lambdas.slice(0, 1), points.slice(0, 2))
    val g1 = points(0) * lambdas(0) + (1 - lambdas(0)) * points(1)
    println(r1 + " " + g1)
    assertTrue(math.abs(r1 - g1) < Stats.DoubleEpsilon)

    val r2 = Stats.linearInterpolation(lambdas.slice(0, 2), points.slice(0, 3))
    val g2 = points(0) * lambdas(0) + (1 - lambdas(0)) * (lambdas(1) * points(1) + (1 - lambdas(1)) * points(2))
    assertTrue(math.abs(r2 - g2) < Stats.DoubleEpsilon)

    val r3 = Stats.linearInterpolation(lambdas.slice(0, 3), points.slice(0, 4))
    val g3 = points(0) * lambdas(0) + (1 - lambdas(0)) * (lambdas(1) * points(1) + (1 - lambdas(1)) *
      (lambdas(2) * points(2) + (1 - lambdas(2)) * points(3)))
    assertTrue(math.abs(r3 - g3) < Stats.DoubleEpsilon)
  }


  @Test
  def testEntropy() {
    var f = List(("a", 10), ("b", 0))
    assertTrue(math.abs(0.0 - Stats.entropy(f)) < Stats.DoubleEpsilon)
    var p = List(1.0, 0.0)
    assertTrue(math.abs(0.0 - Stats.entropy(p)) < Stats.DoubleEpsilon)

    p = List(0.0)
    assertTrue(math.abs(0.0 - Stats.entropy(p)) < Stats.DoubleEpsilon)
    p = List(1.0)
    assertTrue(math.abs(0.0 - Stats.entropy(p)) < Stats.DoubleEpsilon)

    f = List(("a", 10), ("b", 10))
    assertTrue(math.abs(1.0 - Stats.entropy(f)) < Stats.DoubleEpsilon)
    p = List(0.5, 0.5)
    assertTrue(math.abs(1.0 - Stats.entropy(p)) < Stats.DoubleEpsilon)

    f = List(("a", 4), ("b", 2), ("c", 1), ("d", 1))
    assertTrue(math.abs((7.0/4.0) - Stats.entropy(f)) < Stats.DoubleEpsilon)
    p = List(1.0/2, 1.0/4, 1.0/8, 1.0/8)
    assertTrue(math.abs((7.0/4.0) - Stats.entropy(p)) < Stats.DoubleEpsilon)
  }

}
