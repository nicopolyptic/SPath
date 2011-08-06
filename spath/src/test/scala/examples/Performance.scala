package examples

import xspath.XSPathLite
import xml._
import java.util.Date
import management.{MemoryMXBean, MemoryPoolMXBean}
import scala.collection.JavaConversions

object Performance extends XSPathLite {
  def main(args: Array[String]) {

    val root = tree(3, 10)

    var sb = new StringBuilder
    new PrettyPrinter(1000, 3).format(root, sb)
    //   println(sb)

    val A = Element("A")

    val cache: Unit => Unit = _ => {
      val query = \\(A)
      val result = $$(query)(root)
      println(result.size)
    }

    val nocache: Unit => Unit = _ => {
      val query = \\(A)
      val result = $(query)(root)
      println(result.size)
    }

    val scala: Unit => Unit = _ => root \\ "A"

    val hand: Unit => Unit = _ => {
      val result = all(IndexedSeq.apply(root), IndexedSeq.apply(root))
      println(result.size)
    }


//    println("SPath cache")
//    time(cache)
//    printMemoryUsage


//    println("Hand coded")
//    time(hand)
//    printMemoryUsage

    println("SPath no cache")
    time(nocache)
    printMemoryUsage

//    println("Scala")
//    time(scala)
//    printMemoryUsage


  }

  def time(f: Unit => Unit) {
    val before = new Date()
    f()
    val after = new Date()
    println((after.getTime - before.getTime) / 1000d)
  }

  def tree(breadth: Int, depth: Int): Node = {

    if (depth == 0)
      return <A></A>

    var children: NodeSeq = NodeSeq.Empty

    for (i <- 1 to breadth) {
      val child = tree(breadth, depth - 1)
      children = children ++ child
    }

    return <A>{children}</A>
  }

  def all(ns: IndexedSeq[Node], r: IndexedSeq[Node]): IndexedSeq[Node] = {
    val c = ns.flatMap(n => n.child)
    if (c.length == 0)
      r
    else all(c, r ++ c.filter(n => n.label == "A"))
  }

  def printMemoryUsage = {

    val beans: java.util.List[MemoryPoolMXBean] = java.lang.management.ManagementFactory.getMemoryPoolMXBeans();
    for (bean <- JavaConversions.asScalaIterable(beans)) {
      println(bean.getName() + " " + bean.getPeakUsage().getUsed().toDouble / 1048576d)
    }

    val bean: MemoryMXBean = java.lang.management.ManagementFactory.getMemoryMXBean()
    println("heap " + bean.getHeapMemoryUsage().getUsed().toDouble / 1048576d)

  }
}