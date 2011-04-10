package swingspathlite

import spath.SPathLite
import swing.{Container, UIElement}

class SwingSPathLite extends SPathLite[UIElement] {
    override def childAxis = (n : UIElement) => u match {case c : Container => c.contents case _ => empty}
}