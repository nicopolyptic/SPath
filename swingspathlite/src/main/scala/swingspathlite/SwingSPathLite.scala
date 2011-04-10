package swingspathlite

import spath.SPathLite
import swing.{Container, UIElement}
import collection.immutable.VectorBuilder

class SwingSPathLite extends SPathLite[UIElement] {
    override def childAxis = (n : UIElement) =>
      n match {
        case c : Container => (new VectorBuilder[UIElement]() ++= c.contents).result
        case _ => empty
      }
}