package spath

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
  final def ?[C](cls: Class[C]): Predicate = ?(n => cls.isAssignableFrom(n.getClass()))
  final def nth(i: Int) = ?(n => position(n) == i)

  final val child = children
  final val precedingSibling : axis = n => sibling(position(n) - 1)(n)
  final val followingSibling : axis = n => sibling(position(n) + 1)(n)
  final val ancestorOrSelf = $(\\(parent))
  final val descendantOrSelf = $(\\(child))
  final val ancestor = $(\(parent)\\parent)
  final val descendant = $(\(child)\\child)
  final val following = $(\\(parent)\followingSibling\\followingSibling\\child)
  final val preceding = $(\\(parent)\precedingSibling\\precedingSibling\\child)

  final val empty = new Vector[T](0, 0, 0)

  final val sibling : Int => axis = i => n => {
    val siblings = parent(n).headOption match {
      case Some(v) => children(v)
      case None => empty
    }
    if (siblings.isDefinedAt(i))
      List(siblings.apply(i))
    else empty
  }

  final def position(n: T): Int =
    parent(n).headOption match {
      case Some(v) => children(v).indexOf(n)
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
    else compose(f, n - 1, s.flatMap(n => f(n)))

  final def descendants(i: Int): axis = n => compose(child, i, List(n))

  final def not(e : Query) = e match {
    case Predicate(p) => ?(n => !p(n))
    case e : Query => ?(n => $(n, e).size == 0)
  }

  final def exists(e : Query) = ?(n => $(n, e).size > 0)
}
