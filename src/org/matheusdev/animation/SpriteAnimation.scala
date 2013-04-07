package org.matheusdev.animation


/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/27/13
 * Time: 5:07 PM
 */
class SpriteAnimation[I <: Image[I,_]](val data: AnimationData[I], val repeating: Boolean) {

  var currentFrame: I = data.frames.head.region
  var currentTime: Float = 0f
  lazy val totalDelay = data.frames.foldLeft(0f)((lastDelay, frame) => frame.delay + lastDelay)

  def tick(delta: Float) {
    currentTime += delta
    val frameTime = if (repeating) currentTime % totalDelay else currentTime

    var walkedDelay = 0f

    currentFrame = data.frames.head.region

    for (frame <- data.frames if walkedDelay < frameTime) {
      walkedDelay += frame.delay
      currentFrame = frame.region
    }
  }

}
