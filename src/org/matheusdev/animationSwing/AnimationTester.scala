package org.matheusdev.animationSwing

import swing._
import event.ButtonClicked
import org.matheusdev.animation.{ImageColor, ResourceLoader, XmlResourceLoader, SpriteAnimation}
import xml.XML
import javax.swing.JOptionPane
import javax.swing.filechooser.FileFilter
import java.io.{Closeable, IOException, File}
import org.matheusdev.util._
import javax.imageio.stream.{FileImageOutputStream, IIOByteBuffer, ImageOutputStream}
import java.awt.image.BufferedImage
import javax.swing.border.EmptyBorder

/*
 * Created with IntelliJ IDEA.
 * Author: matheusdev
 * Date: 3/28/13
 * Time: 5:37 PM
 */
object AnimationTester extends SimpleSwingApplication {
  var currentFile: File = null
  var currentAnim: String = null
  var currentResLoader: XmlResourceLoader[SwingImage] = null
  val swingImageLoader = new SwingImageLoader()

  val viewCanvas = new ViewCanvas()
  val loadButton = new Button("Load Resource File")
  val reloadButton = new Button("Reload")
  val changeAnimation = new Button("Change Animation")
  val exportToGif = new Button("Export to Gif")
  val packFrame = new Button("Pack Frame")
  val animationList = new ComboBox[String](Seq())

  val flowPanel = new FlowPanel(FlowPanel.Alignment.Leading)(
    loadButton,
    reloadButton,
    changeAnimation
  )
  val flowPanelExtra = new FlowPanel(FlowPanel.Alignment.Leading)(
    exportToGif,
    packFrame
  )
  flowPanel.maximumSize = flowPanel.preferredSize
  flowPanelExtra.maximumSize = flowPanelExtra.preferredSize
  val boxPanel = new BoxPanel(Orientation.Vertical) {

    contents += flowPanel
    contents += flowPanelExtra
    contents += viewCanvas

    border = Swing.EmptyBorder(8)
  }
  val frame = new MainFrame {

    title = "Animation Tester"
    contents = boxPanel
  }
  def top: Frame = frame

  def loadAnimation(file: File, name: String) = {
    viewCanvas.animation({
      val res = new XmlResourceLoader[SwingImage](new SwingImageLoader())
      res.load(XML.loadFile(file), file.getParentFile)
      new SpriteAnimation[SwingImage](res.animation(name), true)
    })
  }

  def newXmlFileChooser() = {
    val fileChooser = new FileChooser()
    fileChooser.multiSelectionEnabled = false
    fileChooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
    fileChooser.fileFilter = new FileFilter {
      def getDescription: String = "Xml Files"
      def accept(f: File): Boolean = {
        f.getName.endsWith(".xml") || f.isDirectory
      }
    }
    fileChooser
  }

  def executeFileChooserResult[R](fileChooser: FileChooser, dialogName: String, iferror: R)(execute: File => R) = {
    fileChooser.showDialog(viewCanvas, dialogName) match {
      case FileChooser.Result.Approve => execute(fileChooser.selectedFile)
      case FileChooser.Result.Cancel => iferror
      case FileChooser.Result.Error => iferror
    }
  }

  listenTo(loadButton, reloadButton, exportToGif, changeAnimation)
  reactions += {
    case ButtonClicked(`loadButton`) => {
      val fileChooser = newXmlFileChooser()
      val animation = executeFileChooserResult(fileChooser, "Open Xml", null.asInstanceOf[SpriteAnimation[SwingImage]]) { file =>
        val animationName = JOptionPane.showInputDialog("Name of the Animation in resource xml: ")
        val scale = JOptionPane.showInputDialog("Scale of the image: ").toFloat

        val res = new XmlResourceLoader[SwingImage](swingImageLoader)
        res.load(XML.loadFile(file), file.getParentFile)
        val anim = res.animation(animationName)
        if (anim == null) {
          JOptionPane.showMessageDialog(null, "Couldn't find animation with name "+animationName+" in xml.")
          null
        } else {
          currentFile = file
          currentAnim = animationName
          currentResLoader = res
          viewCanvas.scale(scale)
          new SpriteAnimation[SwingImage](anim, true)
        }
      }
      viewCanvas.animation(animation)
      frame.pack()
    }
    case ButtonClicked(`reloadButton`) => {
      viewCanvas.animation({
        val res = new XmlResourceLoader[SwingImage](new SwingImageLoader())
        currentResLoader = res
        res.load(XML.loadFile(currentFile), currentFile.getParentFile)
        new SpriteAnimation[SwingImage](res.animation(currentAnim), true)
      })
    }
    case ButtonClicked(`changeAnimation`) => {
      val newAnimation = JOptionPane.showInputDialog("New animation Name:")
      val scale = JOptionPane.showInputDialog("How many times to scale the image up?").toInt
      currentAnim = newAnimation
      viewCanvas.animation(new SpriteAnimation[SwingImage](currentResLoader.animation(newAnimation), true))
      viewCanvas.scale(scale)
      frame.pack()
    }
    // Gif EXPORT
    case ButtonClicked(`exportToGif`) => {
      val white = new ImageColor(1, 1, 1, 1)
      def blend(src: ImageColor, dst: ImageColor) = {
        val srcA = src.a
        val omSrcA = 1-srcA
        new ImageColor(
          src.r * srcA + dst.r * omSrcA,
          src.g * srcA + dst.g * omSrcA,
          src.b * srcA + dst.b * omSrcA,
          math.max(src.a, dst.a))
      }
      val fileChooser = new FileChooser()
      fileChooser.multiSelectionEnabled = false
      fileChooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
      val file = fileChooser.showDialog(null, "Export Gif") match {
        case FileChooser.Result.Approve => fileChooser.selectedFile
        case _ => null
      }
      if (file != null) {
        val delay = JOptionPane.showInputDialog("How many milliseconds between each frame?").toInt
        val scale = JOptionPane.showInputDialog("How many times to scale the image up?").toInt
        println("Choosen file: "+file+", delay: "+delay+", scale: "+scale)

        withCloseable(new FileImageOutputStream(file)) { stream =>
          val writer = new GifSequenceWriter(stream, BufferedImage.TYPE_INT_ARGB, delay, true)
          withCloseable (writer) { writer =>
            println("Writing frames")
            for (frame <- viewCanvas.animation.data.frames) {
              val newFrame = swingImageLoader.createEmptyImage(frame.region.width * scale, frame.region.height * scale)
              frame.region.foreach { (x, y, color) =>
                for {
                  xx <- 0 until scale
                  yy <- 0 until scale
                } {
                  val newColor = blend(color, white)
                  newFrame.pixel(x * scale + xx, y * scale + yy, newColor)
                }
              }
              println("Writing frame...")
              writer.writeToSequence(newFrame.backingImage)
            }
          }
        }
      }
    }
    case ButtonClicked(`packFrame`) => frame.pack()
  }
}

class ViewCanvas extends Component {

  ignoreRepaint = true

  private var anim: SpriteAnimation[SwingImage] = null
  private var scale: Float = 1f

  def animation(anim: SpriteAnimation[SwingImage]) {
    if (anim != null) {
      val size = new Dimension((anim.currentFrame.width * scale).toInt, (anim.currentFrame.height * scale).toInt)
      preferredSize = size
      minimumSize = size
      revalidate()
    }
    this.anim = anim
  }

  def animation = anim

  def scale(s: Float) {
    if (anim != null) {
      val size = new Dimension((anim.currentFrame.width * scale).toInt, (anim.currentFrame.height * scale).toInt)
      preferredSize = size
      minimumSize = size
      revalidate()
    }
    this.scale = s
  }

  val thread = {
    val thread = new Thread(new Runnable {
      def run() {
        while (true) {
          if (anim != null) {
            anim.tick(0.016f)
          }
          repaint()
          Thread.sleep(16)
        }
      }
    })
    thread.setDaemon(true)
    thread.start()
    thread
  }

  override def paint(g2d: Graphics2D) {
    if (anim != null) {
      val frame = anim.currentFrame
      frame.draw(g2d, 0, 0, frame.width * scale, frame.height * scale)
    }
  }

}
