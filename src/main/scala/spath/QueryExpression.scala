package spath

trait QueryExpression[T] {

  type axis = T => Iterable[T]
  type predicate = T => Boolean
  final def * = Predicate((o: T) => true)
  def defaultAxis: axis
  def SPath(e: Query) = Query.SPath(e)

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
    def \(e : Query) : Query = \(defaultAxis, e)
  }

  case class Predicate(p: T => Boolean) extends Query {
    def apply(o : T) = p(o)
    override def toString = "predicate"
  }

  case class And(val l: Query, val r: Query) extends Query {
    override def toString = "(" + l + " /\\ " + r + ")"
  }

  case class Or(val l: Query, val r: Query) extends Query {
    override def toString = "(" + l + " \\/ " + r + ")"
  }

  case class Until(f: axis, val l: Query, val r: Query) extends Query {
    override def toString = "(" + l + " U " + r + ")"
  }

  case class X(f: axis, val next: Query) extends Query {
    override def toString = "X ( " + next + " )"
  }

  private object Query {
    def SPath(e: Query): Boolean = {
      e match {
        case _: Predicate => true
        case Or(l, r) => SPath(l) && SPath(r)
        case And(l, r) => (hasNoAxes(l)&& SPath(r)) || (SPath(l) && hasNoAxes(r))
        case X(_, next) => SPath(next)
        case Until(_, l, r) => hasNoAxes(l) && SPath(r)
      }
    }

    def hasNoAxes(e : Query) :Boolean = {
      e match {
        case p: Predicate => true
        case Or(l, r) => hasNoAxes(l) && hasNoAxes(r)
        case And(l, r) => hasNoAxes(l) && hasNoAxes(r)
        case X(_, next) => false
        case Until(_, l, r) => false
      }
    }
  }
}