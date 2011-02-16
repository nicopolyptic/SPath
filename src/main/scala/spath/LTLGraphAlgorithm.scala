package sq

import annotation.tailrec
import collection.mutable.{Stack, HashSet, Set}

trait LTLGraphAlgorithm[T <: AnyRef] extends Expression[T] {

  private def createGraph(e: Expr): Node = Algorithm.createGraph(e)

  def evaluate(e: Expr): T => Set[T] = {
    val n = createGraph(e)
    o => evaluate(o, n.next(o))
  }


  protected def startEvaluation = {}
  protected def endEvaluation = {}


  private def evaluate(rootObject: T, nodes: Set[Node]): Set[T] = {

    startEvaluation

    val resultSet = new HashSet[T]
    val stack = new Stack[Tuple3[T,Node,Set[T]]]

    for (node <- nodes)
      stack push ((rootObject, node, new HashSet[T]))

    while (stack.size > 0) {

      val state = stack.pop()
      if (state._2.isSelected()) state._3 += state._1
      if (state._2.isAccepting()) resultSet ++= state._3
      else {
        val axis = state._2.axis
        for (o <- axis(state._1))
          for (nd <- state._2.next(o))
            stack push ((o, nd, state._3.clone))
      }
    }

    endEvaluation
    resultSet
  }

  private def evaluate2(rootObject: T, nodes: Set[Node]): Set[T] = {

    val resultSet = new HashSet[T]
    val stack = new Stack[Tuple4[T,Node,Set[T],Set[State]]]

    val visitedStates = new HashSet[State]()
    val acceptingPathStates = new HashSet[State]()

    for (node <- nodes)
      stack push ((rootObject, node, new HashSet[T], new HashSet[State]))

    while (stack.size > 0) {

      val state = stack.pop()
      val visitedState = new State(state._1, state._2)
      visitedStates += visitedState
      state._4 += visitedState

      if (state._2.isSelected()) state._3 += state._1
      if (state._2.isAccepting()) {
        resultSet ++= state._3
        acceptingPathStates ++= state._4
      } else {
        val axis = state._2.axis
        for (o <- axis(state._1))
          for (nd <- state._2.next(o)) {
            val visitedState = new State(o, nd)
            if (visitedStates.contains(visitedState)) {
              if (acceptingPathStates.contains(visitedState))
                resultSet ++= state._3
            } else
              stack push ((o, nd, state._3.clone, state._4.clone))
          }
      }
    }
    resultSet
  }

  private class State(val o: T, val node: Node) {

    override def hashCode = java.lang.System.identityHashCode(o) + node.name.hashCode * 31

    override def equals(obj: Any) =
      obj match {
        case v: State => v.node.name.equals(node.name) && (v.o eq o)
      }
  }


  private object Algorithm {
    var i: Int = 0

    def newName = {val k: Int = i; i += 1; k.toString}

    def expand(n: Node, nl: Set[Node]): Set[Node] = {
      if (n.New.size == 0) {
        var nd = n.findNode5(nl)

        if (nd != null) {
          nd.incoming ++= n.incoming
          return nl
        }
        else
          {
            nl += n
            //  if (n.next.size > 0) {

            var newn = new Node(newName)
            newn.incoming += n
            newn.New ++= n.next
            return expand(newn, nl)
            //          } else
            //            return nl
          }
      }
      else {

        val a: Array[Expr] = new Array[Expr](n.New.size)
        n.New.copyToArray(a)
        val eta: Expr = a(0)
        n.New -= eta

        eta match {
          case _: Predicate =>
            n.old += eta
            return expand(n, nl)
          case _: Until =>
            var n1 = new Node(newName)
            n1.New += n.new1(eta)
            n1.New --= n.old
            n1.New ++= n.New

            n1.incoming ++= n.incoming
            n1.old ++= n.old += eta
            n1.next ++= n.next
            n1.next += n.next1(eta)

            var n2 = new Node(newName)
            n2.New += n.new2(eta)
            n2.New --= n.old
            n2.New ++= n.New

            n2.incoming ++= n.incoming
            n2.old ++= n.old += eta
            n2.next ++= n.next

            return expand(n1, expand(n2, nl))

          case And(l, r) =>
            if (!n.old.contains(l))
              n.New += l
            if (!n.old.contains(r))
              n.New += r
            n.old += eta

            return expand(n, nl)

          case x: X =>
            n.old += eta
            n.next += x.next
            return expand(n, nl)
        }
      }
    }

    def createGraph(e: Expr): Node = {

      val n = new Node(newName)
      val i = new Node("init")
      n.incoming += i
      n.New += e
      val ns = expand(n, new HashSet[Node])

      for (n <- ns)
        for (i <- n.incoming)
          i.outgoing += n
      i
    }

    private def print(l: Set[Node]) {
      for (n <- l)
        n.print;
      println("")
    }
  }

  private class Node(val name: String) {
    type node = Node
    type expr = Expr

    val incoming: Set[node] = new HashSet[node]
    val New: Set[expr] = new HashSet[expr]
    val old: Set[expr] = new HashSet[expr]
    val next: Set[expr] = new HashSet[expr]
    val outgoing = new HashSet[node]

    def findNode5(l: Set[node]) = {
      var node: node = null

      for (n <- l)
        if (n.old == this.old && n.next == this.next)
          node = n
      node;
    }

    def new1(e: expr) = e match {
      case u: Until => u.l
      case _ => null
    }

    def new2(e: expr) = e match {
      case u: Until => u.r
      case _ => null
    }

    def next1(e: expr) = e match {
      case u: Until => u
      case _ => null
    }

    def isAccepting() = next.size == 0

    def isSelected() = isSelectedAux(old.toArray, 0)

    def isSelectedAux(es: Array[expr], i: Int): Boolean = {

      if (i > es.size - 1) false
      else
        es(i) match {
          case p: Predicate => if (p.selected) true else isSelectedAux(es, i + 1)
          case _ => isSelectedAux(es, i + 1)
        }
    }

    def next(o: T) = {
      var result: Set[node] = new HashSet[node]
      nextAux(o, outgoing.toArray, 0, result)
      result
    }

    @tailrec
    private def nextAux(a: T, s: Array[node], i: Int, result: Set[node]): Unit = {
      if (i > s.size - 1) return
      if (s(i) isSatisfiedBy a) result add s(i)
      nextAux(a, s, i + 1, result)
    }

    def oldContains(e: Expr): Boolean = {
      for (oldE <- old) {
        if (e eq oldE)
          return true
      }
      return false
    }

    def axis: axis = {
      if (old.size == 0)
        (o: T) => new HashSet[T]

      for (e <- old) {
        e match {
          case X(f, _) => return f
          case _ =>
        }
      }

      for (e <- old) {
        e match {
          case Until(f, _, r) => if (!oldContains(r)) return f
          case _ =>
        }
      }

      (o: T) => new HashSet[T]
    }

    def print = {

      println("\t" + name + ":")
      println("\t in:")
      incoming foreach (e => println("\t\t" + e.name + "\n"))
      println("\t old:")
      old foreach (e => println("\t\t" + e))
      println("\t next:")
      next foreach (e => println("\t\t" + e))
    }

    def isSatisfiedBy(a: T): Boolean = {
      for (e <- old) {
        e match {
          case p: Predicate => if (!p.apply(a)) return false
          case _ =>
        }
      }
      return true
    }
  }
}