package examples

import _root_.xspath.XSPathLite

object BranchingTimeExample extends XSPathLite {

  val A = elem("A")

  def main(args: Array[String]): Unit = {

    val doc =
      <R>
        <B><A/><A/></B>
        <C>
            <A/>
            <D/>
        </C>
      </R>

    val result = $(doc, \\(not(exists(\(not(A))))))
    println(result)
  }
}