package examples

import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import org.xml.sax.ErrorHandler
import java.io.FileInputStream
import xsq.XSQ
import org.w3c.dom.{Node, Document}

object EmployeeXSQ extends XSQ {

  def employee : Predicate = element("employee")
  def employee(p : Predicate) : Expr = employee and p
  def name : Predicate = element("name")
  def id : Predicate = element("id")
  def managerId : Predicate = element("managerId")
  def name(txt : String) : Predicate = ?(e => $(\(name)\ text(txt))(e).size > 0)

  def manager : axis = employeeNode =>

    $(\(managerId)\ text)(employeeNode).headOption match {
      case Some(managerId) =>
        $(\(parent)\ select(employee)
                \id\ text(managerId.getNodeValue)) (employeeNode)
      case None => empty
    }

  def main(args: Array[String]): Unit = {

    val factory = DocumentBuilderFactory.newInstance()
    val builder = factory.newDocumentBuilder()
    val document = builder.parse(this.getClass.getResourceAsStream("employees.xml"))
    val query = \\(employee(name("John"))) \\(manager, not(\(managerId))) \ name \ text

    $(query)(document).headOption match {
      case Some(managerName) =>
        println(managerName.getNodeValue)
    }
  }
}



