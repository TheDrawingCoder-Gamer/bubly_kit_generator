package net.bulbyvr.swing.svg;

import scala.swing.* 
import org.apache.batik.swing.svg.{JSVGComponent, GVTTreeBuilderListener, GVTTreeBuilder, GVTTreeBuilderEvent, SVGDocumentLoaderListener, SVGDocumentLoaderEvent}
import org.apache.batik.gvt.GraphicsNode
import java.net.URL
import org.w3c.dom.svg.SVGDocument
class SVGComponent extends Component {
  override lazy val peer : JSVGComponent = new JSVGComponent() with SuperMixin
  def loadDocument(url : String) : Unit = {
    peer.loadSVGDocument(url)
  }
  def svgDocument_=(doc : SVGDocument) = {
    peer.setSVGDocument(doc)
  }
  def svgDocument = 
    peer.getSVGDocument()
  object gvtTreeBuilder extends Publisher {
    import net.bulbyvr.swing.svg.{event => bevent}
    peer.addGVTTreeBuilderListener(new GVTTreeBuilderListener {
      def gvtBuildCancelled(e : GVTTreeBuilderEvent) = {
        publish(bevent.GVTTreeBuilderCancelled(SVGComponent.this, e.getGVTRoot()))
      }
      def gvtBuildCompleted(e : GVTTreeBuilderEvent) = {
        publish(bevent.GVTTreeBuilderCompleted(SVGComponent.this, e.getGVTRoot()))
      }
      def gvtBuildFailed(e : GVTTreeBuilderEvent) = {
        publish(bevent.GVTTreeBuilderFailed(SVGComponent.this, e.getGVTRoot()))
      }
      def gvtBuildStarted(e : GVTTreeBuilderEvent) = {
        publish(bevent.GVTTreeBuilderStarted(SVGComponent.this, e.getGVTRoot()))
      }
    })
  }
  object svgDocumentLoader extends Publisher {
    import net.bulbyvr.swing.svg.{event => bevent}
    peer.addSVGDocumentLoaderListener(new SVGDocumentLoaderListener {
      def documentLoadingCancelled(e : SVGDocumentLoaderEvent) = {
        publish(bevent.SVGDocumentLoaderCancelled(SVGComponent.this, e.getSVGDocument()))
      }
      def documentLoadingCompleted(e : SVGDocumentLoaderEvent) = {
        println("hi 1")
        publish(bevent.SVGDocumentLoaderCompleted(SVGComponent.this, e.getSVGDocument()))
      }
      def documentLoadingFailed(e : SVGDocumentLoaderEvent) = {
        publish(bevent.SVGDocumentLoaderFailed(SVGComponent.this, e.getSVGDocument()))
      }
      def documentLoadingStarted(e : SVGDocumentLoaderEvent) = {
        publish(bevent.SVGDocumentLoaderStarted(SVGComponent.this, e.getSVGDocument()))
      }
    })
  }
}

package event {
  object GVTTreeBuilderEvent {
    def unapply(e : GVTTreeBuilderEvent) : Option[(SVGComponent, GraphicsNode)] = 
      Some((e.source, e.node))
  }
  abstract class GVTTreeBuilderEvent(override val source : SVGComponent, val node : GraphicsNode) extends swing.event.UIEvent
  class GVTTreeBuilderCancelled(override val source : SVGComponent, override val node : GraphicsNode) extends GVTTreeBuilderEvent(source, node)
  class GVTTreeBuilderCompleted(override val source : SVGComponent, override val node : GraphicsNode) extends GVTTreeBuilderEvent(source, node)
  class GVTTreeBuilderFailed(override val source : SVGComponent, override val node : GraphicsNode) extends GVTTreeBuilderEvent(source, node)
  class GVTTreeBuilderStarted(override val source : SVGComponent, override val node : GraphicsNode) extends GVTTreeBuilderEvent(source, node)
  abstract class SVGDocumentLoaderEvent(override val source : SVGComponent, val svgDocument : SVGDocument) extends swing.event.UIEvent
  case class SVGDocumentLoaderCancelled(override val source : SVGComponent, override val svgDocument : SVGDocument) extends SVGDocumentLoaderEvent(source, svgDocument)
  case class SVGDocumentLoaderCompleted(override val source : SVGComponent, override val svgDocument : SVGDocument) extends SVGDocumentLoaderEvent(source, svgDocument)
  case class SVGDocumentLoaderFailed(override val source : SVGComponent, override val svgDocument : SVGDocument) extends SVGDocumentLoaderEvent(source,svgDocument)
  case class SVGDocumentLoaderStarted(override val source : SVGComponent, override val svgDocument : SVGDocument) extends SVGDocumentLoaderEvent(source, svgDocument)
  // abstract class UpdateManagerEvent(override val source : SVGComponent, val image : BufferedImage) extends swing.event.UIEvent 
  }
