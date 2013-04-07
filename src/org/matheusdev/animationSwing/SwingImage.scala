package org.matheusdev.animationSwing

import java.awt.{Color, Graphics}
import org.matheusdev.animation.{ImageLoader, ImageColor, Image}
import java.awt.image.BufferedImage
import collection.mutable
import org.matheusdev.animation.filter.{FilterKernelType, FilterArguments, FilterArgumentParser, Filter}
import org.matheusdev.util.FastBlurFilter

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/28/13
 * Time: 5:29 PM
 */

class SwingImage(private val img: BufferedImage) extends Image[SwingImage, Graphics] {

  def draw(graphicsContext: Graphics, x: Float, y: Float, w: Float, h: Float) {
    graphicsContext.drawImage(img, x.toInt, y.toInt, w.toInt, h.toInt, Color.WHITE, null)
  }

  def getSubimage(x: Int, y: Int, w: Int, h: Int) =
    new SwingImage(img.getSubimage(x, y, w, h))

  def width = img.getWidth
  def height = img.getHeight

  def backingImage = img

  protected def pixelChecked(x: Int, y: Int) = {
    val col = new Color(img.getRGB(x, y), true)
    new ImageColor(col.getRed / 255f, col.getGreen / 255f, col.getBlue / 255f, col.getAlpha / 255f)
  }
  protected def pixelChecked(x: Int, y: Int, to: ImageColor) {
    val col = new Color(to.r, to.g, to.b, to.a)
    img.setRGB(x, y, col.getRGB)
  }
}
