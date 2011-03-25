package swingspath

import spath._

import collection.mutable.WrappedArray.ofRef

import java.awt.{Component, Container}

trait SwingSPath extends SPath[Component] {

  override def children = (c : Component) =>
    c match {
      case t: Container => new ofRef(t.getComponents())
      case _ => empty
    }

  override def parent = (c : Component) =>
    if (c.getParent() != null)
      List(c.getParent())
    else List()
}