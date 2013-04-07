package org.matheusdev.animation.filter

import org.matheusdev.animation.{ImageLoader, ImageColor, Image}
import scala.concurrent._
import duration.Duration
import ExecutionContext.Implicits.global

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/29/13
 * Time: 12:31 AM
 */
class Filter[I <: Image[I,_],A]( val name: String,
                                 val argParser: FilterArgumentParser[A],
                                 val kernelType: FilterKernelType[I],
                                 val filter: (Int, Int, I, A, FilterKernelType[I]) => ImageColor) {

  def apply(loader: ImageLoader[I], source: I, filterArgs: String) = {
    val args = argParser.parseArgs(source.width, source.height, filterArgs)

    val arg = args.argumentData
    val width = args.newWidth
    val height = args.newHeight

    val destination = loader.createEmptyImage(width, height)

    for (ftr <- {
      for {
        x <- 0 until destination.width
        y <- 0 until destination.height
      } yield future {
        destination.pixel(x, y, filter(x, y, source, arg, kernelType))
      }
    }) Await.result(ftr, Duration.Inf)
    destination
  }
}

class FilterArguments[A](val argumentData: A, val newWidth: Int, val newHeight: Int)

trait FilterArgumentParser[A] {
  def parseArgs(imageWidth: Int, imageHeight: Int, args: String): FilterArguments[A]

  def parseColor(args: String, filterName: String) = {
    val splitted = args.split(",\\s*")
    try {
      var r = splitted(0).toFloat
      var g = splitted(1).toFloat
      var b = splitted(2).toFloat
      var a = splitted(3).toFloat
      if (r > 2f) r /= 255f
      if (g > 2f) g /= 255f
      if (b > 2f) b /= 255f
      if (a > 2f) a /= 255f
      new ImageColor(r, g, b, a)
    } catch {
      case e: Exception => {
        throw new IllegalArgumentException(
          "Illegal arguments to filter \""+filterName+"\": "+args+". " +
          "Must be 4 Integers seperated by commas: "+e)
      }
    }
  }
}

class FilterKernelType[I <: Image[I,_]](val wrapping: Boolean) {
  def getKernel(image: I, x: Int, y: Int, extWidth: Int, extHeight: Int) = {
    if (extWidth == 0 || extHeight == 0)
      new Kernel(image.pixel(x, y), extWidth, extHeight, IndexedSeq())
    else
      new Kernel(image.pixel(x, y), extWidth, extHeight,
        for {
          kernelX <- (x-extWidth) to (x+extWidth)
          kernelY <- (y-extHeight) to (y+extHeight)
          // The middle pixel of the kernel is handled externally
          if (kernelX != x || kernelY != y)
          // If it's not wrapping, it'll check for validation of the position:
          if (wrapping || image.isPosValid(kernelX, kernelY))
        } yield {
          // If it's not wrapping it won't even get here, when it shouldn't:
          image.pixel(clampPositive(kernelX, image.width), clampPositive(kernelY, image.height))
        })
  }

  private def clampPositive(value: Int, range: Int) = {
    var v = value
    while (v < 0) v += range
    v % range
  }
}

class Kernel(val mid: ImageColor, val extWidth: Int, val extHeight: Int, val surrounding: IndexedSeq[ImageColor]) {
  def foreach(itr: (Int,Int,ImageColor) => Unit) {
    val width = extWidth*2+1
    val height = extHeight*2+1
    var rest = surrounding
    for {
      x <- 0 until width
      y <- 0 until height
      if (rest != Nil)
    } {
      if (x == extWidth && y == extHeight)
        itr(x, y, mid)
      else {
        itr(x, y, rest.head)
        rest = rest.tail
      }
    }
  }
}