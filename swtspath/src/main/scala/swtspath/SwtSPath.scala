package swtpath

import spath.SPath
import org.eclipse.swt.widgets.{Composite, Control}

class SwtSPath extends SPath[Control] {
  override def children = (n : Control) => n match {case c : Composite => c.getChildren case _ => empty}
  override def parent = (n : Control) => {
    val p = n.getParent
    if (p == null) empty else List(p)
  }
}