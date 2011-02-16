package swingsq

import sq._

import collection.mutable.WrappedArray.ofRef

import java.awt.{Component, Container}

trait SwingSQ extends SQ[Component] {

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