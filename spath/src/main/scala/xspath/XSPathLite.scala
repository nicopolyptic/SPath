package xspath

import xml.{Text, Elem, Node}
import spath.{IdentityWrapper, SPathLite}

trait XSPathLite extends SPathLite[Node] {

  override def childAxis = (n: Node) => n.child.toIndexedSeq

  def % = ?(n =>
      n match {
        case _: Elem => true
        case _ => false
      })

  private def text(value :String): predicate = n =>
    n match {
      case t: Text => t.text == value
      case _ => false
  }

  private def >= (value : Int): predicate = n =>
    n match {
      case t: Text =>  try {t.text.toDouble >= value.toDouble} catch {case e : NumberFormatException => false}
      case _ => false
  }

  private def < (value : Int): predicate = n =>
    n match {
      case t: Text => try {
        t.text.toDouble < value.toDouble
      } catch {
        case e : NumberFormatException => false
      }
      case _ => false
  }

  private def text: predicate = n =>
    n match {
      case _: Text => true
      case _ => false
    }

  val txt : Query = Predicate(text)

  private def textValue(n: Node) : String =
    n match {
      case t: Text => t.text
    }

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

  def @@(attributeName : String) : Node => String = n => @@(attributeName, n)

  def exists(attributeName : String) : predicate = n => ?@(attributeName, n) match {case None => false case Some(s) => true}

  class Attribute(name : String) extends Predicate(exists(name)) with Function[Node, String] {
    def == (value : String) = Predicate(n => XSPathLite.this.@@(name, n) == value)
    def @@ (n : Node) : String = XSPathLite.this.@@ (name, n)
    override def apply(n:Node) : String = @@(n)
    // join Attribute/Attribute
    def on (a : Attribute, e : Query) = Predicate(n => $(n, e insert Predicate(m => this @@ n == a @@ m)).size > 0)
    // join same Attribute/Attribute
    def on (e : Query) : Predicate = on (this, e)
    //join Attribute/Element
    def join (e : Query) = Predicate(n => $(n, e insert \(Predicate(text(name)))).size > 0)

    def < (i: Int) = Predicate(n => try {@@(n).toDouble < i.toDouble}catch{case e:NumberFormatException => false})
  }

  object Attribute {
    def apply(name:String) = new Attribute(name)
  }

  class Element(val label : String) extends Predicate(element(label)) with Function[Predicate, Query] {
    override def apply(e:Predicate) :Query = e and this
  }

  object Element {
    def apply(label: String) = new Element(label);
  }

  def element : predicate = n => n match {case _: Elem => true case _ => false}
  def elem = ?(element)

  def element(label: String) : predicate =
    n =>
      n match {
        case _: Elem => n.label == label
        case _ => false
      }

  def sum(e:Query) = (n:Node) => {
    val r = $(n,e\Predicate(text))
    if (r.size > 0)
      r map (n => textValue(n).toDouble) reduceLeft (_+_)
    else 0
  }

  def last(e:Query) : axis = n => $$(n,e).lastOption match{case Some(v) => List(v) case _ => empty}
  def nth(i: Int, e:Query) : axis = range(i,i,e)
  private def range(from:Int, to:Int, e:Query) : axis = n => $$(n,e).slice(from-1,to)
  def slice(head : Int, tail: Int, e : Query) : axis = n => {val r = $$(n,e); r.slice(head, r.size - tail)}

  def number(e:Query) = (n:Node) => $(n, e\Predicate(text)).headOption match {case Some(n) => textValue(n).toDouble case _ => 0}
  def numberOption(e:Query) = (n:Node) => $(n, e\Predicate(text)).headOption match {case Some(n) => Some(textValue(n).toDouble) case _ => None}
  def text(e:Query) : Node => String = (n:Node) => $(n, e\Predicate(text)).headOption match {case Some(n) => textValue(n) case _ => ""}

  class EnhancedQuery(q:Query) {
    def \\+ (q2 : Query) = q\(child)\\(child, q2)
    def \\+ (f: axis, q2 : Query) = q\(f)\\(f, q2)
    def \\+ (f: axis) = q\(f)\\(f)

    // join Element/Element
    def join (e : Query): Predicate = ?(
      n => !( ($(n, q\(txt)) map textValue).toSet intersect ($(n, e\(txt)) map textValue).toSet ).isEmpty
    )

    def >= (i: Int) : Query = q insert \(Predicate(XSPathLite.this >= i))
    def < (i: Int)  : Query= q insert \(Predicate(XSPathLite.this < i))
    def <> (s : String) : Query = q insert \( Predicate(n => !XSPathLite.this.text(s)(n)))

    def < (q2:Query) = Predicate(n => {
      numberOption(q)(n) match {
        case Some(l) => numberOption(q2)(n) match {case Some(r) => l < r case None => false}
        case None => false
      }
    })

    def <= (q2:Query) = Predicate(n => {
      numberOption(q)(n) match {
        case Some(l) => numberOption(q2)(n) match {case Some(r) => l <= r case None => false}
        case None => false
      }
    })

    def > (q2:Query) = Predicate(n => number(q)(n) > number(q2)(n))
    def >= (q2:Query) = Predicate(n => number(q)(n) >= number(q2)(n))

    def <= (i:Int) = Predicate(n => number(q)(n) <= i)
    def > (i:Int) = Predicate(n => number(q)(n) > i)
  }

  implicit def enhancedQuery(q:Query) = new EnhancedQuery(q)

  var idIndex = new scala.collection.mutable.HashMap[String, Node]()

  protected override def process(n:Node) : Unit = {
    n.attributes.asAttrMap.get("id") match {case Some(s) => idIndex += s -> n case _ => }
    for (n1 <- children(n)) {
      parentRelation += IdentityWrapper(n1) -> n
    }
  }

  def $id(a : Attribute) : axis = n => idIndex.get (a @@ n) match  {
    case Some(o) => List(o)
    case None => empty
  }

  def $id(e : Element) : axis = $id(Attribute(e.label))

  override val following : axis = n => {
    val doc = $(n, \\(parent, root)).head
    inverseDocIds.get(doc) match {
      case Some(index) => index.drop(docIds(doc)(IdentityWrapper(n)) + 1)
      case None => super.following(n)
    }
  }

  override val preceding : axis = n => {
    val doc = $(n, \\(parent, root)).head
    inverseDocIds.get(doc) match {
      case Some(index) => index.dropRight(index.size - docIds(doc)(IdentityWrapper(n)))
      case None => super.preceding(n)
    }
  }
}