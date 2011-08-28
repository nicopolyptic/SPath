package examples

import _root_.xspath.XSPathLite
import java.util.Date

object NaiveExample2 extends XSPathLite {

  val A = Element("A")
  val B = Element("B")
  val doc =
      <A>
          <B/>
          <B/>
      </A>

  def main(args: Array[String]): Unit = {
    var before = new Date()
    var after = new Date()
    var q = \(B) \(parent, A)
    var r = $(doc, q)

    for (i <- 1 to 1000) {
      before = new Date()
      r = $(doc, q)
      after = new Date()
      println((after.getTime - before.getTime) / 1000d)
      q = q \ B \(parent, A)
    }
  }
}
