package examples

import xsq.XSQLite
import xml.Node

object AttrEmployeeXSQLite extends XSQLite {

  def employee: Predicate = element("employee")

  def employee(p: Predicate): Expr = employee and p

  def name(txt: String): Predicate = ?(e => $(\(text(txt)))(e).size > 0)

  def id(txt: String): Predicate = \@("id", txt)

  def managerId(txt: String): Predicate = \@("managerId", txt)

  def managerId = \@("managerId")

  def manager: axis = e =>
    managerId(e) match {
      case Some(mid) => $(\(parent) \ employee(id(mid)))(e)
      case None => empty
    }

  def topManager = ?(n => managerId(n) match {
    case None => true case _ => false
  })

  def main(args: Array[String]): Unit = {

    val departments =

      <organisation>

        <employees>
            <employee id="4">CEO</employee>
        </employees>

        <department id="0">

          <employees>
            <employee id="4" managerId="2">John</employee>
            <employee id="2" managerId="3">Jack</employee>
            <employee id="3">Jill</employee>
          </employees>

          <department id="1">
            <employees>
              <employee id="1" managerId="2">John</employee>
              <employee id="2" managerId="3">Jack</employee>
              <employee id="3" managerId="4">Jill</employee>
            </employees>
          </department>

          <department id="2">
            <employees>
              <employee id="1" managerId="2">John</employee>
              <employee id="2" managerId="3">Jack</employee>
              <employee id="3">Jill</employee>
            </employees>
          </department>

        </department>

      </organisation>

    val employees =
      <employees>
        <employee id="1" managerId="2">John</employee>
        <employee id="2" managerId="3">Jack</employee>
        <employee id="3">Jill</employee>
      </employees>



    val query = \\(employee(name("John"))) \\ (manager, topManager) \ text


    $(query)(employees).headOption match {

      case Some(managerName) =>
        println(managerName.text)
    }
  }
}



