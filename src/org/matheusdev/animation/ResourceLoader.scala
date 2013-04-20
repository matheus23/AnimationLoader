package org.matheusdev.animation

import collection.mutable
import filter._
import java.io.File
import scala.Some
import org.matheusdev.properties.FileProperty

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/29/13
 * Time: 12:09 PM
 */
trait ResourceLoader[I <: Image[I,_],L] {

  protected val regions = new mutable.HashMap[String, I]()
  protected val animations = new mutable.HashMap[String, AnimationData[I]]()

  def region(name: String) = regions.get(name).getOrElse(
    throw new IllegalArgumentException(s"There is no region available for the name '$name'"))

  def animation(name: String) = animations.get(name).getOrElse(
    throw new IllegalArgumentException(s"There is no animation available for the name '$name'"))

  def allAnimations = animations
  def allRegions = regions

  def load(loadingSource: L, directory: File)
}

object ResourceLoader {
  def defaultFilters[I <: Image[I,_]] = {
    val map = new mutable.HashMap[String, Filter[I,_]]()

    /////////////////////////////////////////////
    //            COLORIZE FILTER              //
    /////////////////////////////////////////////
    // Multiplies the image with a specific value
    map.put("colorize", new Filter[I,ImageColor]("colorize",
      // Filter Arguments: (i. e. "color(1, 1, 0.0, 0.1)"
      new FilterArgumentParser[ImageColor] {
        def parseArgs(imageWidth: Int, imageHeight: Int, args: String) =
          new FilterArguments(parseColor(args, "colorize"), imageWidth, imageHeight)
      }, new FilterKernelType[I](wrapping = false),
      // Filter itself: from (one color & colorize color (from "FilterArgumentParser" above)) => another color
      (x: Int, y: Int, source: I, arg: ImageColor, kernelType: FilterKernelType[I]) => {
        val col = source.pixel(x, y)
        new ImageColor(
          arg.r*col.r,
          arg.g*col.g,
          arg.b*col.b,
          arg.a*col.a)
      }))
    /////////////////////////////////////////////
    //      COLORIZE GRAYSCALE FILTER          //
    /////////////////////////////////////////////
    // Same as "colorize", but processes only grayscale colors
    map.put("colorize_grayscale", new Filter[I,ImageColor]("colorize_grayscale",
    // Filter Arguments: (i. e. "color(1, 1, 0.0, 0.1)"
    new FilterArgumentParser[ImageColor] {
      def parseArgs(imageWidth: Int, imageHeight: Int, args: String) =
        new FilterArguments(parseColor(args, "colorize_grayscale"), imageWidth, imageHeight)
    }, new FilterKernelType[I](wrapping = false),
      // Filter itself: from (one color & colorize color (from "FilterArgumentParser" above)) => another color
      (x: Int, y: Int, source: I, arg: ImageColor, kernelType: FilterKernelType[I]) => {
        val col = source.pixel(x, y)
        // Only apply a colorize filter, when the color is a grayscale color!
        if (col.a > 0.01f && col.isGrayscale)
          new ImageColor(
            arg.r*col.r,
            arg.g*col.g,
            arg.b*col.b,
            arg.a*col.a)
        else
          col
      }))
    /////////////////////////////////////////////
    //           OUTLINE FILTER                //
    /////////////////////////////////////////////
    // outlines the image with a specific color
    class OutlineArgs(val thickness: Int, val color: ImageColor)
    map.put("outline",
      new Filter[I,OutlineArgs]("outline",
        new FilterArgumentParser[OutlineArgs] {
          def parseArgs(imageWidth: Int, imageHeight: Int, args: String) = {
            val strings = args.split("\\s+with\\s+")
            try {
              val thickness = strings(0).toInt
              val color = parseColor(strings(1), "outline")
              new FilterArguments(new OutlineArgs(thickness, color), imageWidth, imageHeight)
            } catch {
              case e: Exception =>
                throw new IllegalArgumentException(
                  "Illegal arguments to filter \"outline\": "+args+". " +
                  "Must be 1 Int and a color seperated by the keyword 'with': "+e)
            }
          }
        }, new FilterKernelType[I](wrapping = false),
          (x: Int, y: Int, source: I, arg: OutlineArgs, kernelType: FilterKernelType[I]) => {
            val kernel = kernelType.getKernel(source, x, y, arg.thickness, arg.thickness)
            // If we're part of the sprite:
            // (which means a non-filled pixel)
            if (kernel.mid.a != 0f)
              kernel.mid
            else {
              // if we have a surrounding pixel being part of the sprite:
              if (kernel.surrounding.exists((c) => c.a != 0f))
                arg.color
              else
                kernel.mid
            }
      }))
    /////////////////////////////////////////////
    //              BLUR FILTER                //
    /////////////////////////////////////////////
    // blurs the image with the given gauss matrix
    class GaussArgs(val matrix: GaussMatrix, val xradius: Int, val yradius: Int)
    map.put("blur",
      new Filter[I,GaussArgs]("blur",
      new FilterArgumentParser[GaussArgs] {
        def parseArgs(imageWidth: Int, imageHeight: Int, args: String) = {
          val strings = args.split(",\\s*")
          try {
            val extWidth = strings(0).toInt
            val extHeight = strings(1).toInt
            val sigmaSqr = strings(2).toFloat
            new FilterArguments(new GaussArgs(new GaussMatrix(extWidth, extHeight, sigmaSqr), extWidth, extHeight), imageWidth, imageHeight)
          } catch {
            case e: Exception => {
              throw new IllegalArgumentException(
                "Illegal arguments to filter \"blur\": "+args+". " +
                "Must be 2 Ints and 1 Float seperated by commas: "+e)
            }
          }
        }
      }, new FilterKernelType[I](wrapping = false),
        (x: Int, y: Int, source: I, gaussArgs: GaussArgs, kernelType: FilterKernelType[I]) => {
          var r = 0f
          var g = 0f
          var b = 0f
          var a = 0f
          var gaussSum = 0f
          val gauss = gaussArgs.matrix
          val kernel = kernelType.getKernel(source, x, y, gaussArgs.xradius, gaussArgs.yradius)
          kernel.foreach((x, y, value) => {
            val gaussVal = gauss(x, y)
            gaussSum += gaussVal
            r += value.r * gaussVal
            g += value.g * gaussVal
            b += value.b * gaussVal
            a += value.a * gaussVal
          })
          new ImageColor(r / gaussSum, g / gaussSum, b / gaussSum, a / gaussSum)
        }))
    /////////////////////////////////////////////
    //           REPLACEMENT FILTER            //
    /////////////////////////////////////////////
    // finds and replaces every color value with a given other color
    class ReplaceColors(val replace: ImageColor, val withColor: ImageColor, val epsilon: Float)
    map.put("replace",
      new Filter[I,ReplaceColors]("replace",
      new FilterArgumentParser[ReplaceColors] {
        def parseArgs(imageWidth: Int, imageHeight: Int, args: String) = {
          val strings = args.split("\\s+(with|range)\\s+")
          try {
            val replace = parseColor(strings(0), "replace")
            val withColor = parseColor(strings(1), "replace")
            val epsilon = {
              val floatval = strings(2).toFloat
              if (floatval > 1f) floatval / 255f
              else floatval
            }
            new FilterArguments(new ReplaceColors(replace, withColor, epsilon), imageWidth, imageHeight)
          } catch {
            case e: Exception => {
              throw new IllegalArgumentException(
                "Illegal arguments to filter \"replace\": "+args+". " +
                "Must be of the form of: (R, G, B, A with R, G, B, A range E) " +
                "where R, G, B, A and E (epsilon) are floats: "+e)
            }
          }
        }
      }, new FilterKernelType[I](wrapping = false),
        (x: Int, y: Int, source: I, replace: ReplaceColors, kernelType: FilterKernelType[I]) => {
          val col = source.pixel(x, y)
          if (col.equalsEpsilon(replace.replace, replace.epsilon))
            replace.withColor
          else
            col
        }))
    /////////////////////////////////////////////
    //            Scale Filter                 //
    /////////////////////////////////////////////
    // Copies the image to a bigger scaled Image.
    class ScaleDims(val scalex: Int, val scaley: Int)
    map.put("scale",
      new Filter[I,ScaleDims]("scale",
        new FilterArgumentParser[ScaleDims] {
          def parseArgs(imageWidth: Int, imageHeight: Int, args: String) = {
            val strings = args.split(",\\s*")
            try {
              val scalex = strings(0).toInt
              val scaley = strings(1).toInt
              new FilterArguments(new ScaleDims(scalex, scaley), imageWidth * scalex, imageHeight * scaley)
            } catch {
              case e: Exception => {
                throw new IllegalArgumentException(
                  "Illegal arguments to filter \"scale\": "+args+". " +
                  "Must be 2 Ints indicating x-scaling and y-scaling: "+e)
              }
            }
          }
        }, new FilterKernelType[I](wrapping = false),
        (x: Int, y: Int, source: I, dims: ScaleDims, kernelType: FilterKernelType[I]) => {
          source.pixel(x / dims.scalex, y / dims.scaley)
        }))
    /////////////////////////////////////////////
    //           Expand Filter                 //
    /////////////////////////////////////////////
    // Adds an empty border to the image.
    class ExpandDims(val addX: Int, val addY: Int)
    map.put("expand",
      new Filter[I,ExpandDims]("expand",
        new FilterArgumentParser[ExpandDims] {
          def parseArgs(imageWidth: Int, imageHeight: Int, args: String) = {
            val strings = args.split(",\\s*")
            try {
              val expandX = strings(0).toInt
              val expandY = strings(1).toInt
              new FilterArguments(new ExpandDims(expandX, expandY), imageWidth + expandX*2, imageHeight + expandY*2)
            } catch {
              case e: Exception => {
                throw new IllegalArgumentException(
                  "Illegal arguments to filter \"scale\": "+args+". " +
                  "Must be 2 Ints indicating x-border and y-border in pixels: "+e)
              }
            }
          }
        }, new FilterKernelType[I](wrapping = false),
        (x: Int, y: Int, source: I, dims: ExpandDims, kernelType: FilterKernelType[I]) => {
          val newx = x - dims.addX
          val newy = y - dims.addY
          if (source.isPosValid(newx, newy))
            source.pixel(newx, newy)
          else
            new ImageColor(1, 1, 1, 0)
        }))
    /////////////////////////////////////////////
    //         Voxelize Filter                 //
    /////////////////////////////////////////////
    // Adds a voxelization effect to each pixel.
    class VoxelizeArgs(val kernelWidth: Int, val kernelHeight: Int, val color: ImageColor)
    map.put("voxelize",
      new Filter[I,VoxelizeArgs]("voxelize",
        new FilterArgumentParser[VoxelizeArgs] {
          def parseArgs(imageWidth: Int, imageHeight: Int, args: String) = {
            val strings = args.split("\\s*with\\s*")
            try {
              val kernelSizes = strings(0).split(",\\s*")
              val kernelWidth = kernelSizes(0).toInt
              val kernelHeight = kernelSizes(1).toInt

              val color = parseColor(strings(1), "voxelize")
              new FilterArguments(new VoxelizeArgs(kernelWidth, kernelHeight, color), imageWidth, imageHeight)
            } catch {
              case e: Exception => {
                throw new IllegalArgumentException(
                  "Illegal arguments to filter \"voxelize\": "+args+". " +
                  "Must be 2 Ints indicating x-border and y-border in pixels: "+e)
              }
            }
          }
        }, new FilterKernelType[I](wrapping = false),
        (x: Int, y: Int, source: I, args: VoxelizeArgs, kernelType: FilterKernelType[I]) => {
          val col = source.pixel(x, y)
          if (x % args.kernelWidth == 0 || y % args.kernelHeight == 0)
            new ImageColor(
              math.max(0, col.r - args.color.r),
              math.max(0, col.g - args.color.g),
              math.max(0, col.b - args.color.b),
              col.a)
          else
            col
        }))
    map
  }

  def defaultStackMergeOps[I <: Image[I,_]] = {
    val map = new mutable.HashMap[String,StackMergeOp[I,_]]()

    sealed abstract class Overlay()
    case class OverlayTopOverBottom() extends Overlay
    case class OverlayBottomOverTop() extends Overlay
    map.put("blend",
      new StackMergeOp[I,Overlay] {
        def parseArgs(args: String) = args.toLowerCase match {
          case "top over bottom" => OverlayTopOverBottom()
          case "bottom over top" => OverlayBottomOverTop()
          case _ =>
            throw new IllegalArgumentException(
              "Arguments for \"blend\" merge need to be either " +
              "\"top over bottom\" or \"bottom over top\"")
        }
        def combinePixels(colTop: ImageColor, colBottom: ImageColor, overlay: Overlay) = {
          def blend(src: ImageColor, dst: ImageColor) = {
            val srcA = src.a
            val omSrcA = 1-srcA
            new ImageColor(
                src.r * srcA + dst.r * omSrcA,
                src.g * srcA + dst.g * omSrcA,
                src.b * srcA + dst.b * omSrcA,
                math.max(src.a, dst.a))
          }
          overlay match {
            case OverlayTopOverBottom() => blend(colTop, colBottom)
            case OverlayBottomOverTop() => blend(colBottom, colTop)
          }
        }
      })
    map.put("overwrite",
      new StackMergeOp[I,Unit] {
        def parseArgs(args: String) = Unit
        def combinePixels(colTop: ImageColor, colBottom: ImageColor, noth: Unit) = {
          colTop
        }
      })
    map
  }
}
