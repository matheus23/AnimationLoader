package org.matheusdev.animation.filter

import xml.Node
import org.matheusdev.animation.{XmlFormatException, ImageLoader, Image}
import collection.mutable
import org.matheusdev.util._

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 4/5/13
 * Time: 1:35 PM
 */
class Push[I <: Image[I,_]](val name: String, val filters: Seq[Either[(Filter[I,_], String),(Push[I], String)]])

class FilterOperations[I <: Image[I,_]](
                                         filtersXml: Node,
                                         val imageLoader: ImageLoader[I],
                                         val filterDepot: mutable.Map[String,Filter[I,_]],
                                         val mergeOps: mutable.Map[String,StackMergeOp[I,_]]) {

  val filters = parsePush(filtersXml)

  def applyFilters(image: I) = {
    val filterStack = new FilterStack[I](imageLoader, image)

    println("<filters>")
    val totalTime = measureTimeMs {
      def applyFilters(pushTouple: (Push[I], String), image: I) {
        val push = pushTouple._1
        for (filterElem <- push.filters) {
          filterElem match {
            case Left((filter: Filter[I,_], arg: String)) =>
              print("<filter operation: \"" + filter.name + "(" + arg + ")\"")
              val time = measureTimeMs {
                filterStack.set(filter.apply(imageLoader, filterStack.get(), arg))
              }
              println(" (" + time + " ms) />")
            case Right((push: Push[I], arg: String)) =>
              println("<push> operation: \"" + push.name + "(" + arg + ")\"")
              val pushTime = measureTimeMs {
                filterStack.push()
                applyFilters((push, arg), filterStack.get())
                filterStack.pop(mergeOps.get(push.name).getOrElse(
                  throw new IllegalArgumentException("Couldn't find merge operation with name \""+push.name+"\".")
                ), arg)
              }
              println("</push (took " + pushTime + " ms)>")
          }
        }
      }
      applyFilters(filters, image)
    }

    println("</filters (took " + totalTime + " ms)>\n")

    filterStack.get()
  }

  def parsePush(pushXml: Node): (Push[I], String) = {
    val filters =
      for (filter <- pushXml.child if (filter.label == "filter" || filter.label == "branch")) yield {
        filter.label match {
          case "filter" =>
            Left(parseFilter(filter))
          case "branch" =>
            Right(parsePush(filter))
          case e =>
            throw new XmlFormatException("Expected either <filter /> or <branch>, instead got "+e+" in <filters> element.")
        }
      }
    if (pushXml.label == "branch") {
      val pushAttrib = findAttribute(pushXml, "merge")
      val pushName = getExpressionName(pushAttrib)
      val pushArgs = getExpressionArg(pushAttrib)
      (new Push[I](pushName, filters), pushArgs)
    } else {
      (new Push[I]("overlay", filters), "top over bottom")
    }
  }

  def parseFilter(filterXml: Node) = {
    val filterDescription = findAttribute(filterXml, "name")
    val filterName = getExpressionName(filterDescription)
    val filter = imageLoader.nativeFilters.get(filterName) match {
      case Some(x) => x
      case None =>
        filterDepot.get(filterName).getOrElse({
          throw new IllegalArgumentException("Couldn't find filter for name \""+filterName+"\" (in "+filterXml+")")
        })
    }
    val filterArg = getExpressionArg(filterDescription)
    (filter, filterArg)
  }

  def getExpressionArg(parserStr: String) = {
    // matches "colorize(blah, () () blah **** ***)" => "blah, () () blah **** ***"
    //                   ^-----------------------^
    parserStr.substring(
      parserStr.indexOf('(') + 1,
      parserStr.lastIndexOf(')'))
  }

  def getExpressionName(parserStr: String) = {
    // matches "colorize(blah, blah **** ***)" => "colorize"
    //          ^------^
    parserStr.substring(
      0,
      parserStr.indexOf('('))
  }

  def findAttribute(xml: Node, attribute: String) = {
    xml.attribute(attribute) match {
      case Some(attr) => attr.text
      case None => throw new XmlFormatException("Expected attribute \""+attribute+"\" in tag "+xml)
    }
  }

  def findAttributeFloat(xml: Node, attribute: String, elseVal: Float) = {
    xml.attribute(attribute) match {
      case Some(attr) => attr.text.toFloat
      case None => elseVal
    }
  }
}
