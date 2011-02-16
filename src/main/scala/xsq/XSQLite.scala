package xsq

import sq.{SQLite}
import xml.{Text, Elem, Node}

trait XSQLite extends SQLite[Node] {

  override def childAxis = (n: Node) => n.child.toIndexedSeq

  def element(label: String): Predicate =
    ?(n =>
      n match {
        case _: Elem => n.label == label
        case _ => false
      })

  def text(txt: String): Predicate =
    ?(n =>
      n match {
        case _: Text => n.text == txt
        case _ => false
      })

  def text: Predicate = ?(n =>
    n match {
      case _: Text => true
      case _ => false
    }
  )

  def \@(attributeName : String) : Node => Option[String] =
    n => n.attributes.asAttrMap.get(attributeName)

  def \@(attributeName : String, value : String) : Predicate =
    ?(n => \@(attributeName)(n) match {
      case Some(attributeValue) => attributeValue == value
      case None => false
    })
}