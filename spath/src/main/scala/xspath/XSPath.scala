package xspath

import spath.SPath
import org.w3c.dom.Node
import scala.collection.immutable.VectorBuilder

trait XSPath extends SPath[Node] {

  override def children = n =>
    if (!n.hasChildNodes) empty
    else {
      var vectorBuilder = new VectorBuilder[Node]()
      val cNodes = n.getChildNodes()
      for (i <- 0 to(cNodes.getLength() - 1)) {
        vectorBuilder += cNodes.item(i)
      }
      vectorBuilder.result
    }

  override def parent = n => {
    val parent = n.getParentNode()
    if (parent == null) empty
    else List(parent)
  }

  def element(tagName: String) =
    ?(n =>n.getNodeType() == Node.ELEMENT_NODE &&
            n.getNodeName() == tagName)

  def text : Predicate = ?(n => n.getNodeType() == Node.TEXT_NODE)
  def text(data: String): Predicate =
    ?(n => text.evaluate(n) && n.getNodeValue() == data)
}


