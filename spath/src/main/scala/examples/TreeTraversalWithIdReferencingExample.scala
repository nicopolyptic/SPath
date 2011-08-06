package examples

import _root_.xspath.XSPathLite

object TreeTraversalWithIdReferencingExample extends XSPathLite {

  val node = Element("node")
  def node(p: Query): Query = node and p
  def name(text: String) = ?@("name", text)
  def id(txt: String) = ?@("id", txt)
  def pid(txt: String) = ?@("pid", txt)

  val parentNode : axis = n => $(n, \(parent)\node(id(@@("pid", n))))
  val childNode : axis = n => $(n, \(parent)\node(pid(@@("id", n))))

  def main(args: Array[String]): Unit = {

    val forest =
      <forest>
        <tree>
          <nodes>
              <node id="1" name="A"/>
              <node id="2" pid="1" name="B"/>
              <node id="3" pid="1" name="C"/>
              <node id="4" pid="2" name="D"/>
              <node id="5" pid="2" name="E"/>
              <node id="6" pid="3" name="F"/>
              <node id="7" pid="3" name="G"/>
          </nodes>
        </tree>
        <tree>
          <nodes>
              <node id="8" name="A"/>
              <node id="9" pid="8" name="B"/>
              <node id="10" pid="8" name="C"/>
          </nodes>
        </tree>
      </forest>


    val result1 = $(forest, \\(node(name("G")))\\(parentNode, not(exists("pid"))))
    println(result1 map @@("name"))


    val result2 = $(forest, \\(node(name("A") and id("1")))\childNode\\childNode)
    println(result2 map @@("name"))
  }
}