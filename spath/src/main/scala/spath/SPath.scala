package spath

import annotation.tailrec
import collection.immutable.{VectorBuilder}
import collection.mutable.{ListBuffer, Stack}

trait SPath[T <: AnyRef] extends QueryExpression[T] with LltAlgorithm[T] {

  def parent: axis
  def children: T => IndexedSeq[T]

  override def defaultAxis = children

  final def ?(p: predicate) : Predicate = Predicate(p)
  final def ?(e : Query) : Query = exists(e)
  final def nth(i: Int) = ?(n => position(n) == i - 1)
  final val first = nth(1)
  final val leftSibling : axis = n => sibling(position(n) - 1)(n)
  final val rightSibling : axis = n => sibling(position(n) + 1)(n)

  final val self : axis = n => List(n)
  final val child = children
  final val descendant = $(\(child)\\child)
  final val descendantOrSelf = $(\\(child))
  final val ancestor = $(\(parent)\\parent)
  final val ancestorOrSelf = $(\\(parent))
  final val followingSibling : axis = $(\(rightSibling)\\rightSibling)
  final val precedingSibling : axis = $(\(leftSibling)\\leftSibling)

  val followingQuery = $(\\(parent)\rightSibling\\rightSibling\\child)
  val precedingQuery = $(\\(parent)\leftSibling\\leftSibling\\child)

//  final val right = $(\(first) or (not(\(first)) and (not(\(rightSibling)) U(parent, \(rightSibling)))))
//  final val left  = $(\(leftSibling, not(leaf) U(child, leaf)) or (not(\(leftSibling)) and \(parent)))

  final val right = $(\(first) or (not(\(first)) and \->(parent, \(rightSibling))))
  final val left  = $(\(leftSibling, \->(child, leaf)) or (not(\(leftSibling)) and \(parent)))

  def following : axis = followingQuery
  def preceding : axis = precedingQuery

  final def root : Predicate = ?(n => parent(n).size == 0)
  final def leaf : Predicate = ?(n => child(n).size == 0)
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

  @tailrec
  private def indexOf(n : T, s: IndexedSeq[T], i : Int) : Int = {
    if (i >= s.size) -1
    else if (IdentityWrapper(n) == IdentityWrapper(s(i))) i
    else indexOf(n, s, i + 1)
  }

  final def position(n: T): Int =
      parent(n).headOption match {
      case Some(v) => indexOf (n, children(v), 0)
      case None => 0
    }

  def $(n : T, e: Query) : Iterable[T] = $(checkForAxis(e))(n)
  def $$(n : T, e: Query) : Iterable[T] = $(checkForAxis(e), true)(n)

  def $(e: Query) : T => Iterable[T] = $(e, false)

  private def $(e: Query, order:Boolean) : T => Iterable[T] = {
    if (!SPath(e))
      throw new Exception("SPath expression contains branching conflicts.")
    val q = buildAutomaton(e)
    (o:T) => {
      val r = q(o);
      if (depth == 0)  {
         automata.clear();
      }
      if (depth == 0 || order) {
        documentOrder(r, o)
      }

      else r
    }
  }

  final private def checkForAxis(e : Query) = e match {case AxisStep(f) => \(f) case _ => e}

  final def compose(f: axis, i : Int) : axis = n => compose(child, i, List(n))

  final def compose(f: axis, n: Int, s: Iterable[T]): Iterable[T] =
    if (n <= 0) s
    else compose(f, n - 1, s.flatMap(n => f(n)))

  final def descendants(i: Int): axis = n => compose(child, i, List(n))

  final def parent(i: Int): axis = n => compose(parent, i, List(n))

  final override def not = (e : Query) => ?(n => $(n, e).size == 0)
  final override def exists = (e : Query) => ?(n => $(n, e).size > 0)
  final override def orderedEval = $$

  @tailrec
  private final def findRoot(n : T) : T = {
    val p = parent(n)
    if (p.size == 0) n else findRoot(p.head)
  }

  def documentOrder(it : Iterable[T], o : T) : Iterable[T] = {
    var di : documentIndex = null
    // defined at root of o not o
    val r = findRoot(o)

    if (!docIds.isDefinedAt(r)) index(r)
    di = docIds.get(r) match {case Some(i) => i}

    val order = (o:T, o1: T) => {
      di(IdentityWrapper(o)) < di(IdentityWrapper(o1))
    }
    var result = ListBuffer[T]()
    result ++= it
    result sortWith order
  }

  /**
   * map each node in the tree rooted at o to its preorder document id.
   */
  def index(o : T) {
    var stack = Stack[T]()
    var map = Map[IdentityWrapper[T], Int]()
    var seq = new VectorBuilder[T]

    stack push o
    var id = 0
    while (stack.size > 0) {
      val item = stack pop()
      seq += item
      map += IdentityWrapper(item) -> id
      id += 1
      process(item)
      for (o1 <- children(item).reverse) {
        stack push o1
      }
    }

    docIds += o -> map
    inverseDocIds += o -> seq.result
  }

  protected def process(o:T) : Unit = ()

  type documentIndex = Map[IdentityWrapper[T], Int]
  type inverseDocumentIndex = IndexedSeq[T]

  protected var docIds = Map[T, documentIndex]()
  protected var inverseDocIds = Map[T, inverseDocumentIndex]()

  def removeIndex(o: T) = {docIds - o; inverseDocIds - o}

  protected var depth = 0
  override def startEvaluation = depth += 1
  override def endEvaluation = depth -=  1
}
