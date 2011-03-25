package spath

import scala.collection.mutable.HashSet
import scala.collection.mutable.Set

trait Expression[T] {
  type axis = T => Iterable[T]
  type predicate = T => Boolean

  final def * = Predicate((o: T) => true)

  def defaultAxis: axis

  def isValidSQExpression(e: Expr) = Expr.SQ(e)

  class Expr {
    def and(e: Expr) = And(this, e)

    def or(e: Expr) = Or(this, e)

    def U(f: axis, e: Expr) = Until(f, this, e)

    def insert(e : Expr) : Expr =
        this match {
          case And(e1, e2) => And(e1, e2 insert e)
          case Or(e1, e2) => Or(e1 insert e, e2 insert e)
          case Until(f2, e1, e2) => Until(f2, e1, e2 insert (e))
          case X(f1, e1) => X(f1, e1 insert (e))
          case _ => this and e
        }

    def \\(f: axis, e: Expr) : Expr = this insert(* U (f, e))

    def \(f: axis, e: Expr) : Expr = this insert(X (f, e))

    def \\(f : axis) : Expr = \\(f, *)

    def \(f : axis) : Expr = \(f, *)

    def \\(e : Expr) : Expr = \\(defaultAxis, e)

    def \(e : Expr) : Expr = \(defaultAxis, e)
  }

  case class Predicate(p: T => Boolean) extends Expr {
    def apply(o : T) = p(o)
    override def toString = "predicate"
  }

  case class And(val l: Expr, val r: Expr) extends Expr {
    override def toString = "(" + l + " /\\ " + r + ")"
  }

  case class Or(val l: Expr, val r: Expr) extends Expr {
    override def toString = "(" + l + " \\/ " + r + ")"
  }

  case class Until(f: axis, val l: Expr, val r: Expr) extends Expr {
    override def toString = "(" + l + " U " + r + ")"
  }

  case class X(f: axis, val next: Expr) extends Expr {
    override def toString = "X ( " + next + " )"
  }

  private object Expr {
    def SQ(e: Expr): Boolean = {
      e match {
        case _: Predicate => true
        case Or(l, r) => SQ(l) && SQ(r)
        case And(l, r) => (hasNoAxes(l)&& SQ(r)) || (SQ(l) && hasNoAxes(r))
        case X(_, next) => SQ(next)
        case Until(_, l, r) => hasNoAxes(l) && SQ(r)
      }
    }

    def hasNoAxes(e : Expr) :Boolean = {
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