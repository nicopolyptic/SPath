package sq

import scala.collection.mutable.HashSet
import scala.collection.mutable.Set

trait Expression[T] {
  type axis = T => Iterable[T]
  type predicate = T => Boolean

  final def * = Predicate((o: T) => true)

  def defaultAxis: axis

  def isValidSQExpression(e: Expr) = Expr.SQ(e)

  def markSelected(e: Expr) = {
    Expr.markSelected(e)
    e
  }

  def selectRightMost(e: Expr) = {
    Expr.selectRightmost(e)
    e
  }

  def isSelected(e: Expr) = Expr.isSelected(e)

  class Expr {
    def and(e: Expr) = And(this, e)

    def or(e: Expr) = Or(this, e)

    def U(f: axis, e: Expr) = Until(f, this, e)

    private def insert(e : Expr) : Expr =
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
    var selected: Boolean = false
    def apply(o : T) = p(o)
    override def toString = "selected = " + selected
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
        case And(l, r) => (axes(l) == 0 && SQ(r)) || (SQ(l) && axes(r) == 0)
        case X(_, next) => SQ(next)
        case Until(_, l, r) => axes(l) == 0 && SQ(r)
      }
    }

    private def axes(e: Expr): Int = {
      e match {
        case _: Predicate => 0
        case Or(l, r) => axes(And(l, r))
        case And(l, r) => axes(l) + axes(r)
        case X(_, next) => axes(next) + 1
        case Until(_, l, r) => axes(l) + axes(r) + 1
      }
    }

    def markSelected(e: Expr): Unit = {
      e match {
        case p: Predicate => p.selected = true
        case Or(l, r) => markSelected(l); markSelected(r)
        case And(l, r) => markSelected(l); markSelected(r)
        case X(_, next) => markSelected(next)
        case Until(_, l, r) => markSelected(l); markSelected(r)
      }
    }

    def hasAxes(e : Expr) :Boolean = {
      e match {
        case p: Predicate => false
        case Or(l, r) => hasAxes(l) || hasAxes(r)
        case And(l, r) => hasAxes(l) || hasAxes(r)
        case X(_, next) => true
        case Until(_, l, r) => true
      }
    }

    def selectRightmost(e : Expr) : Expr = {
      e match {
        case p: Predicate => p.selected = true; p
        case Or(l, r) => selectRightmost(l); selectRightmost(r)
        case And(l, r) => selectRightmost(r)
        case X(f, next) => selectRightmost(next)
        case Until(f, l, r) => selectRightmost(r)
      }
    }

    def isSelected(e : Expr) : Boolean = {
      e match {
        case p: Predicate => p.selected
        case Or(l, r) => isSelected(l) || isSelected(r)
        case And(l, r) => isSelected(l) || isSelected(r)
        case X(f, next) => isSelected(next)
        case Until(f, l, r) => isSelected(l) || isSelected(r)
      }
    }
  }
}