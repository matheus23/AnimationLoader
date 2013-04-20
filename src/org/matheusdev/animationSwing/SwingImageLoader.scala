package org.matheusdev.animationSwing

import org.matheusdev.animation.ImageLoader
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import collection.mutable
import org.matheusdev.animation.filter.Filter
import org.matheusdev.util.FastBlurFilter

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/28/13
 * Time: 5:35 PM
 */
class SwingImageLoader extends ImageLoader[SwingImage] {
  val nativeFilters = new mutable.HashMap[String, Filter[SwingImage, _]]()
  nativeFilters.put("fastblur", new NativeFastBlur())

  val loadedImages = new mutable.HashMap[String, SwingImage]()

  def loadImage(file: File) = new SwingImage(ImageIO.read(file))

  def createEmptyImage(width: Int, height: Int) =
    new SwingImage(new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB))
}

class NativeFastBlur() extends Filter[SwingImage,Int](
  "fastblur",
  // this will be implemented ourself:
  null, null, null) {

  override def apply(loader: ImageLoader[SwingImage], source: SwingImage, filterArgs: String) = {
    try {
      val radius = filterArgs.toInt
      var currentBuffer = 0
      var radiuscounter = radius

      val dst = Array(
        source.copyToImage(loader.createEmptyImage(source.width, source.height)),
        loader.createEmptyImage(source.width, source.height))

      while (radiuscounter >= 2) {
        radiuscounter /= 2
        val fastBlur = new FastBlurFilter(radiuscounter)
        val srcImg = dst(currentBuffer).backingImage
        currentBuffer = if (currentBuffer == 0) 1 else 0
        val dstImg = dst(currentBuffer).backingImage
        fastBlur.filter(srcImg, dstImg)
      }
      dst(currentBuffer)
    } catch {
      case e: Exception =>
        throw new IllegalArgumentException(
          "Illegal arguments to filter \"fastblur\": "+filterArgs+". " +
            "Must be an Int indicating blurring radius: "+e)
    }
  }
}
