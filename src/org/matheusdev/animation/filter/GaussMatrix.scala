package org.matheusdev.animation.filter

import org.matheusdev.util.MatrixArray2

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/29/13
 * Time: 5:38 PM
 */
class GaussMatrix(val extWidth: Int, val extHeight: Int, val sigmaSqr: Float) {
  def this(extWidth: Int, extHeight: Int) = this(extWidth, extHeight, 1)

  private val values = new MatrixArray2[Float](extWidth*2+1, extHeight*2+1)
  private var sum = 0f

  // compute values:
  values.setforeach((x, y, value) => {
    val g = GaussMatrix.gauss(x-extWidth, y-extHeight, sigmaSqr).toFloat
    sum += g
    g
  })

  def apply(x: Int, y: Int) = values(x, y)
}

object GaussMatrix {
  val e = 2.718281828459
  def gauss(x: Int, y: Int, sigmaSqr: Float) =
    (1 /(math.sqrt(2 * math.Pi * sigmaSqr))) * math.pow(e, -((x*x + y*y) / (2 * sigmaSqr)))
}
