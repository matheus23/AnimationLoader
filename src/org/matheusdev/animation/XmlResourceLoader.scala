package org.matheusdev.animation

import collection.mutable
import filter.{FilterOperations, StackMergeOp, Filter}
import xml.{NodeSeq, Node, Elem}
import java.io.File
import collection.mutable.ListBuffer

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/26/13
 * Time: 4:37 PM
 */
class XmlResourceLoader[I <: Image[I,_]](
                                          val imageLoader: ImageLoader[I],
                                          val filterDepot: mutable.Map[String,Filter[I,_]],
                                          val mergeOpDepot: mutable.Map[String,StackMergeOp[I,_]]) extends ResourceLoader[I,Elem] {

  val filtergroups = new mutable.HashMap[String,NodeSeq]()

  def this(imageLoader: ImageLoader[I]) =
    this(imageLoader, ResourceLoader.defaultFilters, ResourceLoader.defaultStackMergeOps)

  def load(xml: Elem, directory: File) {
    for (filters <- xml \ "filters") {
      parseFilterGroups(filters)
    }
    for (image <- xml \ "images" \ "image") {
      parseImage(image, directory)
    }
  }

  private def parseFilterGroups(filtersXml: Node) {
    for (filtergroup <- filtersXml \ "filtergroup") {
      filtergroups.put(findAttribute(filtergroup, "name"), filtergroup.child)
    }
  }

  private def parseImage(imageXml: Node, directory: File) {
    val image = imageLoader.loadImage(new File(directory+"/"+findAttribute(imageXml, "file")))
    for (region <- imageXml \ "region") parseRegion(region, image)
    for (animation <- imageXml \ "animation") parseAnimation(animation, image)
  }

  private def parseRegion(regionXml: Node, image: I) {
    val filterOps = parseFilters(regionXml)
    regions.put(
      findAttribute(regionXml, "name"),
      filterOps match {
        case Some(filterOp) =>
          filterOp.applyFilters(image.getRegionFromBoundsString(findAttribute(regionXml, "bounds")))
        case None =>
          image.getRegionFromBoundsString(findAttribute(regionXml, "bounds"))
      })
  }

  private def parseAnimation(animationXml: Node, image: I) {
    animations.put(findAttribute(animationXml, "name"), {
      val delay = findAttributeFloat(animationXml, "delay", 1)

      val filterOps = parseFilters(animationXml)

      val frames = {
        for (child <- animationXml \ "frame") yield {
          val childDelay = findAttributeFloat(child, "delay", delay)
          val regionUnfiltered = image.getRegionFromBoundsString(findAttribute(child, "bounds"))
          val region = filterOps match {
            case Some(filterOp) => filterOp.applyFilters(regionUnfiltered)
            case None => regionUnfiltered
          }
          new Frame(region, childDelay)
        }
      }
      new AnimationData[I](frames)
    })
  }

  private def parseFilters(parentXml: Node) = {
    val filtersXmlList = parentXml \ "filters"
    val completeFiltersXmlList = new ListBuffer[Node]
    for (filterXml <- filtersXmlList \ "_") {
      if (filterXml.label == "filtergroup") {
        val filterGroupName = findAttribute(filterXml, "name")
        completeFiltersXmlList.append(filtergroups.get(filterGroupName).getOrElse(
          throw new IllegalArgumentException("No filtergroup exists for the name \""+filterGroupName+"\"")
        ):_*)
      } else {
        completeFiltersXmlList += filterXml
      }
    }
    val filtersXml = completeFiltersXmlList.toList
    val useFilter = !filtersXml.isEmpty

    if (useFilter)
      Some(new FilterOperations[I](<filters>{ completeFiltersXmlList }</filters>, imageLoader, filterDepot, mergeOpDepot))
    else
      None
  }

  private def findAttribute(xml: Node, attribute: String) = {
    xml.attribute(attribute) match {
      case Some(attr) => attr.text
      case None => throw new XmlFormatException("Expected attribute \""+attribute+"\" in tag "+xml)
    }
  }

  private def findAttributeFloat(xml: Node, attribute: String, elseVal: Float) = {
    xml.attribute(attribute) match {
      case Some(attr) => attr.text.toFloat
      case None => elseVal
    }
  }
}