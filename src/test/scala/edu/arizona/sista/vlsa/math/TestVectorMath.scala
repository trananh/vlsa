package edu.arizona.sista.vlsa.math

import junit.framework.Assert._
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/** Tests for VectorMath.
  *
  * @author trananh
  */
class TestVectorMath extends AssertionsForJUnit  {

  @Test
  def testNormalize() {
    var vector = Array(1., 1., 1., 1.)
    VectorMath.normalize(vector).foreach(v => assertTrue(math.abs(.25 - v) < Stats.DoubleEpsilon))

    vector = Array(10., 20., 30., 40.)
    VectorMath.normalize(vector).zipWithIndex.foreach(e => {
      assertTrue(math.abs(vector(e._2) / vector.sum - e._1) < Stats.DoubleEpsilon)
    })
  }

  @Test
  def testMaxColumnIdx() {
    val matrix = Array(
      Array(1.0, 0.5, 0.99, 1.0, 0.7, 0.95),
      Array(0.5, 0.99, 1.0, 0.7, 0.95, 1.0),
      Array(0.99, 1.0, 0.7, 0.95, 1.0, 0.5),
      Array(1.0, 0.7, 0.95, 1.0, 0.5, 0.99),
      Array(0.7, 0.95, 1.0, 0.5, 0.99, 1.0))

    val t1 = VectorMath.maxColumnIdx(matrix)
    val t1Gold = Array(List(0, 3), List(2, 5), List(1, 4), List(0, 3), List(2, 5))
    assertTrue(t1.size == t1Gold.size)
    t1.zip(t1Gold).foreach(pair => {
      assertTrue(pair._1.size == pair._2.size)
      pair._1.zip(pair._2).foreach(p => assertEquals(p._1, p._2))
    })

    val t2 = VectorMath.maxColumnIdx(matrix, rowRange = (1, 4))
    val t2Gold = Array(List(2, 5), List(1, 4), List(0, 3))
    assertTrue(t2.size == t2Gold.size)
    t2.zip(t2Gold).foreach(pair => {
      assertTrue(pair._1.size == pair._2.size)
      pair._1.zip(pair._2).foreach(p => assertEquals(p._1, p._2))
    })

    val t3 = VectorMath.maxColumnIdx(matrix, colRange = (2, 5))
    val t3Gold = Array(List(3), List(2), List(4), List(3), List(2))
    assertTrue(t3.size == t3Gold.size)
    t3.zip(t3Gold).foreach(pair => {
      assertTrue(pair._1.size == pair._2.size)
      pair._1.zip(pair._2).foreach(p => assertEquals(p._1, p._2))
    })
  }

  @Test
  def testMaxRowIdx() {
    val matrix = Array(
      Array(1.0, 0.5, 0.99, 1.0, 0.7, 0.95),
      Array(0.5, 0.99, 1.0, 0.7, 0.95, 1.0),
      Array(0.99, 1.0, 0.7, 0.95, 1.0, 0.5),
      Array(1.0, 0.7, 0.95, 1.0, 0.5, 0.99),
      Array(0.7, 0.95, 1.0, 0.5, 0.99, 1.0))

    val t1 = VectorMath.maxRowIdx(matrix)
    val t1Gold = Array(List(0, 3), List(2), List(1, 4), List(0, 3), List(2), List(1, 4))
    assertTrue(t1.size == t1Gold.size)
    t1.zip(t1Gold).foreach(pair => {
      assertTrue(pair._1.size == pair._2.size)
      pair._1.zip(pair._2).foreach(p => assertEquals(p._1, p._2))
    })

    val t2 = VectorMath.maxRowIdx(matrix, rowRange = (1, 4))
    val t2Gold = Array(List(3), List(2), List(1), List(3), List(2), List(1))
    assertTrue(t2.size == t2Gold.size)
    t2.zip(t2Gold).foreach(pair => {
      assertTrue(pair._1.size == pair._2.size)
      pair._1.zip(pair._2).foreach(p => assertEquals(p._1, p._2))
    })

    val t3 = VectorMath.maxRowIdx(matrix, colRange = (2, 5))
    val t3Gold = Array(List(1, 4), List(0, 3), List(2))
    assertTrue(t3.size == t3Gold.size)
    t3.zip(t3Gold).foreach(pair => {
      assertTrue(pair._1.size == pair._2.size)
      pair._1.zip(pair._2).foreach(p => assertEquals(p._1, p._2))
    })
  }

}
