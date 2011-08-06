package examples

import _root_.xspath.XSPathLite

object NaiveExample2 extends XSPathLite {


  def A = Element("A")
  def B = Element("B")

  def main(args: Array[String]): Unit = {

    val doc =
      <A>
        <B/>
        <B/>
      </A>

    val r = $$(doc, \(B) \(parent, A) \B \(parent, A) \B \(parent, A)\B \(parent, A)\B \(parent, A)\B \(parent, A) \B \(parent, A) \B \(parent, A)\B \(parent, A)\B \(parent, A)\B \(parent, A)\B \(parent, A) \B \(parent, A)\B \(parent, A)\B \(parent, A)\B \(parent, A) \B \(parent, A) \B \(parent, A)\B \(parent, A)\B \(parent, A)\B \(parent, A))
    println(r.size)

  }



}