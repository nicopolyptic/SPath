package examples

import xspath.XSPathLite

object ContextExample extends XSPathLite {

  val a = Element("a")
  val b = Element("b")
  val id = Attribute("id")

  def main(args: Array[String]) {

    val doc =
      <x>
        <a id="1">
            <b/>
        </a>
        <a id="2">
            <b/>
        </a>
        <a id="3"/>
        <a id="4">
            <b/>
        </a>
      </x>

    println($(doc, \\(a?(\(b)))$nth(3)))
    println($(doc, \\(a)$nth(3)?(\(b))))
  }
}