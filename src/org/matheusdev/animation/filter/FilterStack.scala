package org.matheusdev.animation.filter

import org.matheusdev.animation.{ImageColor, ImageLoader, Image}
import collection.mutable.Stack
import collection.mutable

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 4/5/13
 * Time: 12:58 AM
 */
class FilterStack[I <: Image[I,_]](val imageLoader: ImageLoader[I], val startImage: I) {

  val imageStack = new mutable.Stack[I]()
  imageStack.push(startImage)

  def push() = {
    val current = imageStack.top
    imageStack.push(current.getSubimage(0, 0, current.width, current.height))
  }

  def pop[A](op: StackMergeOp[I,A], args: String) = {
    val imageTop = imageStack.pop()
    val imageBottom = imageStack.pop()

    imageStack.push(op.combine(imageLoader, imageTop, imageBottom, args))
  }

  def get() = imageStack.top

  def set(image: I) = {
    imageStack.pop()
    imageStack.push(image)
  }

}

trait StackMergeOp[I <: Image[I,_],A] {
  def parseArgs(args: String): A
  def combinePixels(colTop: ImageColor, colBottom: ImageColor, arg: A): ImageColor

  def combine(imageLoader: ImageLoader[I], imageTop: I, imageBottom: I, args: String) = {
    if (!(imageTop.width == imageBottom.width && imageTop.height == imageBottom.height))
      throw new IllegalArgumentException("for combining two pushed layers, both layers have to have the same size!")

    val destination = imageLoader.createEmptyImage(imageTop.width, imageBottom.height)
    val arg = parseArgs(args)

    for {
      x <- 0 until destination.width
      y <- 0 until destination.width
    } {
      destination.pixel(x, y, combinePixels(imageTop.pixel(x, y), imageBottom.pixel(x, y), arg))
    }
    destination
  }
}
