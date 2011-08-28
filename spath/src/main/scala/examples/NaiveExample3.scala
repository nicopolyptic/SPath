package examples

import _root_.xspath.XSPathLite
import java.util.Date

object NaiveExample3 extends XSPathLite {

  val x0 : axis = n => $(n, \\(*))
  val x1 : axis = n => $(n, \(x0)\x0)
  val x2 : axis = n => $(n, \(x1)\x1)
  val x3 : axis = n => $(n, \(x2)\x2)
  val x4 : axis = n => $(n, \(x3)\x3)
  val x5 : axis = n => $(n, \(x4)\x4)
  val x6 : axis = n => $(n, \(x5)\x5)
  val x7 : axis = n => $(n, \(x6)\x6)
  val x8 : axis = n => $(n, \(x7)\x7)
  val x9 : axis = n => $(n, \(x8)\x8)
  val x10 : axis = n => $(n, \(x9)\x9)
  val x11 : axis = n => $(n, \(x10)\x10)
  val x12 : axis = n => $(n, \(x11)\x11)
  val x13 : axis = n => $(n, \(x12)\x12)
  val x14 : axis = n => $(n, \(x13)\x13)
  val x15 : axis = n => $(n, \(x14)\x14)
  val x16 : axis = n => $(n, \(x15)\x15)
  val x17 : axis = n => $(n, \(x16)\x16)
  val x18 : axis = n => $(n, \(x17)\x17)
  val x19 : axis = n => $(n, \(x18)\x18)
  val x20 : axis = n => $(n, \(x19)\x19)

    val doc =
      <A>
        <B>
          <D>
            <E>
            </E>
            <F>
            </F>
          </D>
        </B>
        <C>
          <D>
            <E>
            </E>
            <F>
            </F>
          </D>
        </C>
      </A>

  def main(args: Array[String]): Unit = {
//    printAxes(20)
//    printQueries(20)



      run


  }

  def run = {
    var before = new Date()
    var after = new Date()
    before = new Date()
    $(doc, \(x0))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x1))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x2))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x3))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x4))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x5))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x6))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x7))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x8))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x9))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x10))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x11))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x12))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x13))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x14))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x15))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x16))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x17))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x18))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x19))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)
    before = new Date()
    $(doc, \(x20))
    after = new Date()
    println((after.getTime - before.getTime) / 1000d)

  }


  def printAxes(i : Int) = {
    println("val x0 : axis = n => $(n, \\\\(*))")
    for (j <- 1 to i) {
      println("val x" + j + " : axis = n => $(n, \\(x" + (j-1) + ")\\x" + (j-1) + ")")
    }
  }


  def printQueries(i : Int) = {
    for (j <- 0 to i) {
      println("before = new Date()")
      println("$(doc, \\(x" + j + "))")
      println("after = new Date()")
      println("println((after.getTime - before.getTime) / 1000d)")
    }
  }
}
