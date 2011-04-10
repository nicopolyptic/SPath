package swtpath

import spath.SPath
import org.eclipse.swt.widgets.{Composite, Control}

class SwtSPath extends SPath[Control] {
  override def child = (n : Control) => n match {case c : Composite => c.getChildren case _ => empty}
  override def parent = (n : Control) => n.getParent
}