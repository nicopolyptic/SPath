package examples

import _root_.xspath.XSPathLite
import java.util.Date

object NaiveExample extends XSPathLite {

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

    spath
    //scalaxml
  }

  def scalaxml = {

    var before = new Date()
    var after = new Date()

    var r = doc \\ ("_")

    for (i <- 1 to 1000) {

      before = new Date()

      for (j <- 1 to i) {
        r = r \\ ("_")
      }
      //println(r.size)
      after = new Date()
      println((after.getTime - before.getTime) / 1000d)
    }
  }

  def spath = {
    var before = new Date()
    var after = new Date()
    var q = \\(*)
    var r = $(doc, q)
    var i = 0
    for (i <- 1 to 1000) {
      before = new Date()

      r = $(doc, q)
      //println(r.size)
      after = new Date()
      println((after.getTime - before.getTime) / 1000d)
      q = q \\ *
    }
  }
}