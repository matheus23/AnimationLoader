package org.matheusdev.animation

import filter.Filter
import java.io.File
import collection.mutable

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/28/13
 * Time: 4:36 PM
 */

/**
 * @tparam G The class needed for storing the Graphics context.
 *           In awt for example, it's the "Graphics" class.
 */
trait Image[I <: Image[I,G],G] {
  def draw(graphicsContext: G, x: Float, y: Float, w: Float, h: Float)
  def getSubimage(x: Int, y: Int, w: Int, h: Int): I
  def width: Int
  def height: Int

  def getRegionFromBoundsString(bounds: String): I = {
    val strings = bounds.split("\\s+")
    if (strings.length != 4)
      throw new XmlFormatException("Need 4 seperated integers as definition for \"bounds\" ("+bounds+")")
    try {
      val x = strings(0).toInt
      val y = strings(1).toInt
      val w = strings(2).toInt
      val h = strings(3).toInt

      getSubimage(x, y, w, h)
    } catch {
      case e: NumberFormatException =>
        throw new XmlFormatException("\"bounds\" must only contain integers: "+bounds)
    }
  }

  def pixel(x: Int, y: Int) =
    withRangeCheck(x, y)(() => pixelChecked(x, y))

  def pixel(x: Int, y: Int, to: ImageColor) =
    withRangeCheck(x, y)(() => pixelChecked(x, y, to))

  def foreach(operation: (Int,Int,ImageColor) => Unit) = {
    for {
      x <- 0 until width
      y <- 0 until height
    } {
      operation(x, y, pixel(x, y))
    }
  }

  def withRangeCheck[R](x: Int, y: Int)(func: () => R) = {
    if (isPosValid(x, y))
      func()
    else
      throw new IllegalArgumentException("Range check failed: Position out of range: " +
        "pos: ("+x+", "+y+")"+", dim: ("+width+","+height+")")
  }

  def isPosValid(x: Int, y: Int) = x >= 0 && x < width && y >= 0 && y < height

  def copyToImage(other: I) = {
    foreach { (x, y, col) =>
      other.pixel(x, y, col)
      Unit
    }
    other
  }

  protected def pixelChecked(x: Int, y: Int): ImageColor
  protected def pixelChecked(x: Int, y: Int, to: ImageColor)

}

class ImageColor(val r: Float, val g: Float, val b: Float, val a: Float) {
  def isGrayscale = {
    implicit val epsilon = 0.0001f
    (=~=(r, g) && =~=(g, b))
  }
  def equalsEpsilon(other: ImageColor, epsilon: Float) = {
    implicit val imlcEpsilon = epsilon
      (
        =~=(r, other.r) &&
        =~=(g, other.g) &&
        =~=(b, other.b)
      )
  }
  private def =~=(f0: Float, f1: Float)(implicit epsilon: Float): Boolean = (f0-f1).abs <= epsilon
}

trait ImageLoader[I <: Image[I,_]] {
  val nativeFilters: mutable.HashMap[String,Filter[I,_]]
  def loadImage(file: File): I
  def createEmptyImage(width: Int, height: Int): I
}
