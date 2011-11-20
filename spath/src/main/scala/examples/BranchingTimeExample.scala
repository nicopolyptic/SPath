package examples

import _root_.xspath.XSPathLite

object BranchingTimeExample extends XSPathLite {

  val A = Element("A")
  val B = Element("B")
  val C = Element("C")
  val D = Element("D")

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


    println("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
   var  query :Query =  (* or *) U(child,*)
   $(doc, query)


   query = \\(A?(\(B) or \(parent, C)))\D
       $(doc, query)

   query = \\(A ?(\(B) or \(parent, C))) \ D
   $(doc, query)
  }
}