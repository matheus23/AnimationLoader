package org.matheusdev.util

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/29/13
 * Time: 5:49 PM
 */
trait Matrix2[@specialized E] {

  val width: Int
  val height: Int

  // abstract:
  def asSeq: Seq[E]
  protected def getByIndex(index: Int): E
  protected def setByIndex(index: Int, value: E): Unit

  // Implementation:
  protected def allocationSize = width * height
  protected def arrayPosition(x: Int, y: Int) = withRangeCheck(x, y) { (x*height)+y }
  protected def positionFromIndex(index: Int) = ((index / height), (index % height))

  protected def isValidPosition(x: Int, y: Int) = (x >= 0 && x < width && y >= 0 && y < height)
  protected def withRangeCheck[R](x: Int, y: Int)(operation: => R) = {
    if (isValidPosition(x, y))
      operation
    else
      throw new ArrayIndexOutOfBoundsException("position ("+x+", "+y+") is out of range: ("+width+", "+height+")")
  }

  def apply(x: Int, y: Int) = {
    getByIndex(arrayPosition(x, y))
  }

  def update(x: Int, y: Int, value: E) {
    setByIndex(arrayPosition(x, y), value)
  }

  def foreach(itr: (Int,Int,E) => Unit) {
    for (i <- 0 until asSeq.length) {
      val (x, y) = positionFromIndex(i)
      itr(x, y, getByIndex(i))
    }
  }
  def setforeach(itr: (Int,Int,E) => E) {
    for (i <- 0 until asSeq.length) {
      val (x, y) = positionFromIndex(i)
      update(x, y, itr(x, y, getByIndex(i)))
    }
  }

}

class MatrixArray2[@specialized E: Manifest](val width: Int, val height: Int) extends Matrix2[E] {
  private val backingArray: Array[E] = new Array[E](allocationSize)
  def asSeq = backingArray
  protected def getByIndex(index: Int): E = backingArray(index)
  protected def setByIndex(index: Int, value: E) = backingArray(index) = value
}