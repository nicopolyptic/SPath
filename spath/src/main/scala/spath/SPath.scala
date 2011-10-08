package spath

import collection.mutable.Stack

trait SPath[T <: AnyRef] extends QueryExpression[T] with LltAlgorithm[T] {

  def parent: axis
  def children: T => IndexedSeq[T]

  override def defaultAxis = children

  final def ?(p: predicate) = Predicate(p)
  final def ?[C](cls: Class[C]): Predicate = ?(n => cls.isAssignableFrom(n.getClass()))
  final def nth(i: Int) = ?(n => position(n) == i)

  final val leftSibling : axis = n => sibling(position(n) - 1)(n)
  final val rightSibling : axis = n => sibling(position(n) + 1)(n)

  final val self : axis = n => List(n)
  final val child = children
  final val descendant = $(\(child)\\child)
  final val descendantOrSelf = $(\\(child))
  final val ancestor = $(\(parent)\\parent)
  final val ancestorOrSelf = $(\\(parent))
  final val followingSibling : axis = $(\(followingSibling)\\followingSibling)
  final val precedingSibling : axis = $(\(leftSibling)\\leftSibling)
  final val following = $(\\(parent)\rightSibling\\rightSibling\\child)
  final val preceding = $(\\(parent)\leftSibling\\leftSibling\\child)

  final def root : Predicate = ?(n => parent(n).size == 0)
  final def ~\\ (e : Query)= \\(parent, root)\\e
  final def ~\ (e : Query)= \\(parent, root)\e

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

  def $(e: Query) : T => Iterable[T] = {
    if (!SPath(e))
      throw new Exception("SPath expression contains branching conflicts.")
    val q = buildAutomaton(e)
    (o:T) => {
      val r = q(o);
      if (depth == 0)
        documentOrder(r, o)
      else r
    }
  }

  final def compose(f: axis, n: Int, s: Iterable[T]): Iterable[T] =
    if (n <= 0) s
    else compose(f, n - 1, s.flatMap(n => f(n)))

  final def descendants(i: Int): axis = n => compose(child, i, List(n))

  final def parent(i: Int): axis = n => compose(parent, i, List(n))

  final override def not = (e : Query) => ?(n => $(n, e).size == 0)
  final override def exists = (e : Query) => ?(n => $(n, e).size > 0)

  def documentOrder(it : Iterable[T], o : T) : Iterable[T] = {
    val map = documentOrder(o)
    val order = (o:T, o1: T) => map(IdentityWrapper(o)) < map(IdentityWrapper(o1))
    var result = List[T]()
    result ++= it
    result sortWith order
  }

  /**
   * @return map each node in the tree rooted at o to its preorder document id.
   */
  def documentOrder(o : T) : Map[IdentityWrapper[T], Int] = {
    var stack = Stack[T]()
    var map = Map[IdentityWrapper[T], Int]()
    stack push o
    var id = 0
    while (stack.size > 0) {
      val item = stack pop()
      map += IdentityWrapper(item) -> id
      id += 1
      for (o1 <- children(item).reverse) {
        stack push o1
      }
    }
    map
  }

    protected var depth = 0
    override def startEvaluation = depth += 1
    override def endEvaluation = depth -=  1
}
