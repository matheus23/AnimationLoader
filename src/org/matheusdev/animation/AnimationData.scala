package org.matheusdev.animation

import xml.{Node, Elem}
import java.awt.image.BufferedImage


/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/26/13
 * Time: 4:39 PM
 */
class Frame[I <: Image[I,_]](val region: I, val delay: Float)

class AnimationData[I <: Image[I,_]](val frames: Seq[Frame[I]]) {
}