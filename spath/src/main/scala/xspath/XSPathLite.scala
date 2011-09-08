package xspath

import _root_.spath.SPathLite
import xml.{Text, Elem, Node}

trait XSPathLite extends SPathLite[Node] {

  override def childAxis = (n: Node) => n.child.toIndexedSeq

  def <*> = ?(n =>
      n match {
        case _: Elem => true
        case _ => false
      })

  def txt(txt: String): Predicate =
    ?(n =>
      n match {
        case _: Text => n.text == txt
        case _ => false
      })

  def txt: Predicate = ?(n =>
    n match {
      case _: Text => true
      case _ => false
    }
  )

  def @@(n: Node, attributeName : String) : String = @@(attributeName,n)

  def @@(attributeName : String, n : Node) : String =
    n.attributes.asAttrMap.get(attributeName) match {case Some(s) => s case None => ""}

  def ?@(attributeName : String, n: Node) : Option[String] =
    n.attributes.asAttrMap.get(attributeName)

  def ?@(attributeName : String, value : String) : Predicate =
    ?(n => ?@(attributeName, n) match {
      case Some(attributeValue) => attributeValue == value
      case None => false
    })

  final def ?@(attributeName : String, e : Query) : Predicate =
    ?(n => $(n, e insert (?(m => @@(attributeName, n) == @@(attributeName, m)))).size > 0)

  def @@(attributeName : String) : Node => String = n => @@(attributeName, n)

  def exists(attributeName : String) : predicate = n => ?@(attributeName, n) match {case None => false case Some(s) => true}

  class Attribute(name : String) extends Predicate(exists(name)) with Function[Node, String] {
    def == (value : String) = Predicate(n => XSPathLite.this.@@(name, n) == value)
    def @@ (n : Node) : String = XSPathLite.this.@@ (name, n)
    override def apply(n:Node) : String = @@(n)
    def on (e : Query) : Query = XSPathLite.this ?@(name, e)
  }

  object Attribute {
    def apply(name:String) = new Attribute(name)
  }

  class Element(val label : String) extends Predicate(element(label)) with Function[Query, Query] {
    override def apply(e:Query) :Query = e and this
  }

  object Element {
    def apply(label: String) = new Element(label);
  }

  def element : predicate = n => n match {case _: Elem => true case _ => false}
  override def * = ?(element)

  def element(label: String) : predicate =
    n =>
      n match {
        case _: Elem => n.label == label
        case _ => false
      }

  class EnhancedQuery(q:Query) {
    def \\+ (q2 : Query) = q\(child)\\(child, q2)
  }

  implicit def enhancedQuery(q:Query) = new EnhancedQuery(q)
}