package edu.arizona.sista.vlsa.math

import scala.collection.mutable.ListBuffer

/** Provides some common vector/matrix operations.
  *
  * @author trananh
  */
object VectorMath {

  /** Normalize a sequence of values.
    * @param P Sequence of double values.
    * @return The normalized sequence.
    */
  def normalize(P: Iterable[Double]): Iterable[Double] = {
    val sum = P.sum
    P.map(_ / sum)
  }

  /** For each row in the given 2D matrix, find the index of the column with the maximum value.
    * If there are multiple maximal values, the index of the first from the left is chosen.
    *
    * @param matrix A 2D matrix.
    * @param rowRange The range of rows to consider [start, end).  Defaults to all rows.
    * @param colRange The range of columns to consider [start, end).  Defaults to all columns.
    *
    * @return A row vector specifying the index of the column with the maximum value
    *         for each row position.
    */
  def maxColumnIdx(matrix: Array[Array[Double]],
                   rowRange: (Int, Int) = (0, Integer.MAX_VALUE),
                   colRange: (Int, Int) = (0, Integer.MAX_VALUE)): Array[List[Int]] = {
    // Can't operate an a matrix with an empty dimension
    if (matrix.size == 0 || matrix(0).size == 0)
      return Array[List[Int]]()

    // Find the starting & ending indices
    val (rowStart, rowEnd) = (math.max(0, rowRange._1), math.min(matrix.size, rowRange._2))
    val (colStart, colEnd) = (math.max(0, colRange._1), math.min(matrix(0).size, colRange._2))

    // Initialize the index array with all elements default to the starting column index
    val colIdx = new Array[List[Int]](rowEnd - rowStart)

    // For each row, find the index of the column with the maximum value
    var maxValue = 0.0
    var maxIdx = new ListBuffer[Int]()
    for (i <- rowStart until rowEnd) {
      maxValue = 0.0
      maxIdx = new ListBuffer[Int]()
      for (j <- colStart until colEnd) {
        if (math.abs(matrix(i)(j) - maxValue) < Stats.DoubleEpsilon)
          maxIdx.append(j)
        else if (matrix(i)(j) > maxValue) {
          maxIdx.clear()
          maxIdx.append(j)
          maxValue = matrix(i)(j)
        }
      }
      colIdx(i - rowStart) = maxIdx.toList
    }

    colIdx
  }

  /** For each column in the given 2D matrix, find the index of the row with the maximum value.
    * If there are multiple maximal values, the index of the first from the top is chosen.
    *
    * @param matrix A 2D matrix.
    * @param rowRange The range of rows to consider [start, end).  Defaults to all rows.
    * @param colRange The range of columns to consider [start, end).  Defaults to all columns.
    *
    * @return A column vector specifying the index of the row with the maximum value
    *         for each column position.
    */
  def maxRowIdx(matrix: Array[Array[Double]],
                rowRange: (Int, Int) = (0, Integer.MAX_VALUE),
                colRange: (Int, Int) = (0, Integer.MAX_VALUE)): Array[List[Int]] = {
    // Can't operate an a matrix with an empty dimension
    if (matrix.size == 0 || matrix(0).size == 0)
      return Array[List[Int]]()

    // Find the starting & ending indices
    val (rowStart, rowEnd) = (math.max(0, rowRange._1), math.min(matrix.size, rowRange._2))
    val (colStart, colEnd) = (math.max(0, colRange._1), math.min(matrix(0).size, colRange._2))

    // Initialize the index array with all elements default to the starting row index
    val rowIdx = new Array[List[Int]](colEnd - colStart)

    // For each column, find the index of the column with the maximum value
    var maxValue = 0.0
    var maxIdx = new ListBuffer[Int]()
    for (j <- colStart until colEnd) {
      maxValue = 0.0
      maxIdx = new ListBuffer[Int]()
      for (i <- rowStart until rowEnd) {
        if (math.abs(matrix(i)(j) - maxValue) < Stats.DoubleEpsilon)
          maxIdx.append(i)
        else if (matrix(i)(j) > maxValue) {
          maxIdx.clear()
          maxIdx.append(i)
          maxValue = matrix(i)(j)
        }
      }
      rowIdx(j - colStart) = maxIdx.toList
    }

    rowIdx
  }

}
