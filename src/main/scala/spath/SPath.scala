package spath

import collection.mutable.{Set}

trait SPath[T <: AnyRef] extends QueryExpression[T] with LltAlgorithm[T] {

  def parent: axis
  def children: T => IndexedSeq[T]

  final def \\(f: axis, e: Query): Query = * U (f, e)
  final def \(f: axis, e: Query): Query = X(f, e)
  final def \\(f: axis): Query = \\(f, *)
  final def \(f: axis): Query = \(f, *)

  override def defaultAxis = children
  final def \(e: Query): Query = \(defaultAxis, e)
  final def \\(e: Query): Query = \\(defaultAxis, e)

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

  def $(n : T, e: Query) : Iterable[T] = $(e)(n)
  def $$(n : T, e: Query) : Iterable[T] = $$(e)(n)

  def $(e: Query) : T => Iterable[T] = {
    if (!SPath(e))
      throw new Exception("SPath expression contains branching conflicts.")
    evaluateWithoutCaching(e)
  }

  def $$(e: Query) : T => Iterable[T] = {
    if (!SPath(e))
      throw new Exception("SPath expression contains branching conflicts.")
    evaluateWithCaching(e)
  }

  final def compose(f: axis, n: Int, s: Iterable[T]): Iterable[T] =
    if (n <= 0) s
    else compose(f, n - 1, s.flatMap(o => f(o)))

  final def descendants(n: Int): axis = o => compose(child, n, List(o))

  final def not(e : Query) = e match {
    case Predicate(p) => ?(o => !p(o))
    case e : Query => ?(o => $(o, e).size == 0)
  }

  final def exists(e : Query) = ?(o => $(o, e).size > 0)
}
