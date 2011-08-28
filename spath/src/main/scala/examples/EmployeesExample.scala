package examples

import _root_.xspath.XSPathLite

object EmployeesExample extends XSPathLite {

  val employee = Element("employee")
  val id = Attribute("id")
  val mgrId = Attribute("mgrId")

  val manager : axis = n => $(n, \(parent)\employee(id == mgrId @@ n))
  val reports : axis = n => $(n, \(parent)\employee(mgrId == id @@ n))

  def main(args: Array[String]): Unit = {

    val company =
      <company>
        <department>
          <employees>
              <employee id="1" />
              <employee id="2" mgrId="1" />
              <employee id="3" mgrId="1" />
              <employee id="4" mgrId="2" />
              <employee id="5" mgrId="2" />
              <employee id="6" mgrId="3" />
              <employee id="7" mgrId="3" />
          </employees>
        </department>
        <department>
          <employees>
              <employee id="8" />
              <employee id="9" mgrId="8" />
              <employee id="10" mgrId="8"/>
          </employees>
        </department>
      </company>

    val result1 = $(company, \\(employee(id == "7"))\\(manager, not(mgrId)))
    println(result1 map id)

    val result2 = $(company, \\(employee(id == "1"))\reports\\reports)
    println(result2 map id)
  }
}