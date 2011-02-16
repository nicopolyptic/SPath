package sq

import collection.mutable.{Set}
import annotation.tailrec

trait SQ[T <: AnyRef] extends Expression[T] with LTLGraphAlgorithm[T] {

  def parent: axis
  def children: T => IndexedSeq[T]

  final def \\(f: axis, e: Expr): Expr = * U (f, e)
  final def \(f: axis, e: Expr): Expr = X(f, e)
  final def \\(f: axis): Expr = \\(f, *)
  final def \(f: axis): Expr = \(f, *)

  override def defaultAxis = children
  final def \(e: Expr): Expr = \(defaultAxis, e)
  final def \\(e: Expr): Expr = \\(defaultAxis, e)

  final def select(e: Expr) = markSelected(e)
  final def ?(p: predicate) = Predicate(p)
  final def ?[C](cls: Class[C]): Predicate = ?(o => cls.isAssignableFrom(o.getClass()))
  final def nth(i: Int) = ?(o => position(o) == i)

  final val child = children
  final val precedingSibling : axis = o => sibling(position(o) - 1)(o)
  final val followingSibling : axis = o => sibling(position(o) + 1)(o)
  final val ancestorOrSelf = $(\\(parent,*))
  final val descendantOrSelf = $(\\(child,*))
  final val ancestor = $(\(parent)\\(parent,*))
  final val descendant = $(\(child)\\(child,*))
  final val following = $(\\(parent,*)\(followingSibling)\\(followingSibling)\\(child,*))
  final val preceding = $(\\(parent,*)\(precedingSibling)\\(precedingSibling)\\(child,*))

  final val empty = new Vector[T](0, 0, 0)

  final def sibling(i: Int): axis = o => {
    val siblings = parent(o).headOption match {
      case Some(v) => children(v)
      case None => empty
    }
    if (siblings.isDefinedAt(i))
      List(siblings.apply(i))
    else empty
  }

  final def position(o: T): Int =
    parent(o).headOption match {
      case Some(v) => children(v).indexOf(o)
      case None => 0
    }

  def $(e: Expr) : T => Set[T] = {
    if (!isValidSQExpression(e))
      throw new Exception("SQ expression contains branching conflicts.")
    if (!isSelected(e)) selectRightMost(e)
    evaluate(e)
  }

  final def compose(f: axis, n: Int, s: Iterable[T]): Iterable[T] =
    if (n <= 0) s
    else compose(f, n - 1, s.flatMap(o => f(o)))

  final def descendants(n: Int): axis = o => compose(child, n, List(o))

  final def not(e : Expr) = e match {
    case Predicate(p) => ?(o => !p(o))
    case e : Expr => ?(o => $(e)(o).size == 0)
  }
}
