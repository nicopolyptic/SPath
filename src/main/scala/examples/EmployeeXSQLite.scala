package examples

import xsq.{XSQLite}

object EmployeeXSQLite extends XSQLite {

  def employee: Predicate = element("employee")
  def employee(p: Predicate): Expr = employee and p
  def name: Predicate = element("name")
  def id: Predicate = element("id")
  def managerId: Predicate = element("managerId")
  def name(txt: String): Predicate = ?(e => $(\(name) \ text(txt))(e).size > 0)

  def manager: axis = e =>
    $(\(managerId) \ text)(e).headOption match {
      case Some(managerId) =>
        $(\(parent) \ select(employee) \ id \ text(managerId.text))(e)
      case None => empty
    }



  def main(args: Array[String]): Unit = {

    val employees =
      <employees>
        <employee>
          <name>John</name>
          <id>1</id>
          <managerId>2</managerId>
        </employee>
        <employee>
          <name>Jack</name>
          <id>2</id>
          <managerId>3</managerId>
        </employee>
        <employee>
          <name>Jill</name>
          <id>3</id>
        </employee>
      </employees>

    val query = \\(employee(name("John"))) \\ (manager, not(\(managerId))) \ name \ text

    $(query)(employees).headOption match {
      case Some(managerName) =>
        println(managerName.text)
    }
  }
}



