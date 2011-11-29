package spath

import com.sun.org.apache.xpath.internal.axes.AxesWalker

trait QueryExpression[T] {

  type axis = T => Iterable[T]
  type predicate = T => Boolean
  def * = Predicate(_ => true)
  def defaultAxis: axis
  def SPath(e: Query) = Query.SPath(e)
  def not : Query => Query
  def exists : Query => Predicate

  def orderedEval : (T, Query) => Iterable[T]

  class Query {
    def and(e: Query) = And(this, e)
    def or(e: Query) = Or(this, e)
    def U(f: axis, e: Query) = Until(f, this, e)

    def insert(e : Query) : Query =
        this match {
          case And(e1, e2) => And(e1, e2 insert e)
          case Or(e1, e2) => Or(e1 insert e, e2 insert e)
          case Until(f2, e1, e2) => Until(f2, e1, e2 insert (e))
          case X(f1, e1) => X(f1, e1 insert (e))
          case _ => this and e
        }

    def \\(f: axis, e: Query) : Query = this insert(* U (f, e))
    def \(f: axis, e: Query) : Query = this insert(X (f, e))
    def \\(f : axis) : Query = \\(f, *)
    def \(f : axis) : Query = \(f, *)
    def \\(e : Query) : Query = \\(defaultAxis, e)
    def \\*\(e : Query) : Query = \\(defaultAxis, *)\(e)
    def \*\(e : Query) : Query = \(defaultAxis, *)\(e)
    def \\*\\(e : Query) : Query = \\(defaultAxis, *)\\(e)
    def \*\\(e : Query) : Query = \(defaultAxis, *)\\(e)
    def \* : Query = \(defaultAxis, *)
    def \\* : Query = \\(defaultAxis, *)
    def \->(f : axis, e : Query) = this insert X(f, not(e) U (f, e))

    def \(e : Query) : Query = \(defaultAxis, e)
    def ?(e : Query) : Query = this insert exists(e)
    def ?(p : predicate) : Query = this insert exists(Predicate(p))

    def \(a : AxisStep) : Query = this\(a.f)
    def \\(a : AxisStep) : Query = this\\(a.f)

    def $context(f : contextFilter) : AxisStep = AxisStep(n => f(orderedEval(n, this)))
    def $size(i:Int) : Predicate = $context((s:Int) => s == i)

    def $range(from : Int, to : Int) : AxisStep = $context((s:Iterable[T]) => s.view(from -1, to))
    def $view(inLeft : Int, inRight : Int) = $context((s:Iterable[T]) => s.view(inLeft, s.size - inRight))
    def $head(n : Int) = $range(1, n)
    def $tail(n : Int) = $context((s:Iterable[T]) => s.view(n-1,s.size))
    def $ltrim(n : Int) : AxisStep = $view(n,0)
    def $rtrim(n : Int) : AxisStep = $view(0,n)
    def $nth(i : Int) : AxisStep = $range(i,i)
    def $first = $head(1)
    def $last = $context((s:Iterable[T]) => s.view(s.size-1,s.size))
    def $nth(f : Int => Int) : AxisStep = $context((s:Iterable[T]) => {val i = f(s.size);s.view(i-1, i)})
    def $context(f: Int => Boolean) : Predicate = Predicate(n => f(orderedEval(n, this).size))
  }

  case class Predicate(p: T => Boolean) extends Query {
    def evaluate(n : T) = p(n)
    override def toString = "predicate"
  }

  case class And(val l: Query, val r: Query) extends Query {
    override def toString = "(" + l + " /\\ " + r + ")"
  }

  case class Or(val l: Query, val r: Query) extends Query {
    override def toString = "(" + l + " \\/ " + r + ")"
  }

  case class Until(val f: axis, val l: Query, val r: Query) extends Query {
    override def toString = "(" + l + " U " + r + ")"
  }

  case class X(val f: axis, val next: Query) extends Query {
    override def toString = "X ( " + next + " )"
  }

  final def \\(f: axis, e: Query): Query = * U (f, e)
  final def \(f: axis, e: Query): Query = X(f, e)
  final def \\(f: axis): Query = \\(f, *)
  final def \(f: axis): Query = \(f, *)
  final def \(e: Query): Query = \(defaultAxis, e)
  final def \\(e: Query): Query = \\(defaultAxis, e)
  final def \->(f : axis, e : Query) = X(f, not(e) U (f, e))

  case class AxisStep(val f : axis) extends Query {

    def toQuery :Query = QueryExpression.this.\(f)
    override def \(f1 : axis, e:Query) : Query = toQuery\(f1, e)
    override def \\(f1 : axis, e:Query) : Query = toQuery\\(f1, e)
    override def ?(e:Query) : Query = toQuery?(e)
    override def $context(cf : contextFilter) : AxisStep = AxisStep(n => cf(orderedEval(n, QueryExpression.this.\(this.f))))
    override def toString = "Axis " + f
  }

  type contextFilter = Iterable[T] => Iterable[T]
  type contextPredicate = Iterable[T] => Boolean

  object Query {
    def SPath(e: Query): Boolean = {
      e match {
        case _: Predicate => true
        case Or(l, r) => SPath(l) && SPath(r)
        case And(l, r) => (hasNoAxes(l)&& SPath(r)) || (SPath(l) && hasNoAxes(r))
        case X(_, next) => SPath(next)
        case Until(_, l, r) => hasNoAxes(l) && SPath(r)
        case AxisStep(_) => throw new Exception("Evaluation of Axis is not currently allowed. Please convert to a query with Axis.toQuery")
      }
    }

    def hasNoAxes(e : Query) :Boolean = {
      e match {
        case p: Predicate => true
        case Or(l, r) => hasNoAxes(l) && hasNoAxes(r)
        case And(l, r) => hasNoAxes(l) && hasNoAxes(r)
        case X(_, next) => false
        case Until(_, l, r) => false
        case AxisStep(_) => throw new Exception("Evaluation of Axis is not currently allowed. Please convert to a query with Axis.toQuery")
      }
    }

    def label(e: Query, i: Int, positions : scala.collection.mutable.Map[Predicate, Int]) : Int = {
      e match {
        case p: Predicate => positions += p -> i; i+1
        case And(e1, e2) => label(e2, label(e1, i, positions), positions)
        case Or(e1, e2) => label(e2, label(e1, i, positions), positions)
        case Until(_, e1, e2) => label(e2, label(e1, i, positions), positions)
        case X(_, e) => label(e, i, positions)
        case AxisStep(_) => throw new Exception("Evaluation of Axis is not currently allowed. Please convert to a query with Axis.toQuery")
      }
    }
  }
}