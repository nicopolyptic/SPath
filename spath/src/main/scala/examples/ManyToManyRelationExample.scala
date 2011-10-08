package examples

import xspath.XSPathLite

object ManyToManyRelationExample extends XSPathLite {

  val student  = Element("student")
  val students = Element("students")
  val courses = Element("courses")
  val course = Element("course")
  val tutor = Element("tutor")
  val department = Element("department")
  val tutors = Element("tutors")

  val id = Attribute("id")
  val tid = Attribute("tid")
  val name = Attribute("name")

  def courseAxis : axis = n => n.label match {
    case student.label => $(n, ~\\(courses)\course ?(\\(student(id == id @@ n))))
    case tutor.label => $(n, ~\\(courses)\course(tid == id @@ n))
    case _ => empty
  }

  def tutorAxis : axis = n => n.label match {
    case student.label => $(n, \(courseAxis)\tutorAxis)
    case course.label => $(n, ~\\(tutors)\tutor(id == tid @@ n))
    case _ => empty
  }

  def studentAxis : axis = n => n.label match {
    case tutor.label => $(n, \(courseAxis)\studentAxis)
    case course.label =>
      $(n, ~\(students)\student(id on ~\\(courses)\course(id == id @@ n)\\student))
    case _ => empty
  }

  def main(args: Array[String]): Unit = {
    val department =
      <department>
        <students>
            <student id="1" name="John"/>
            <student id="2" name="Jill"/>
        </students>
        <courses>
          <course id="1" tid="1" name="Object-Oriented Programming">
            <students>
                <student id="1"/>
                <student id="2"/>
            </students>
          </course>
          <course id="2" tid="2" name="Logic">
            <students>
                <student id="1"/>
            </students>
          </course>
        </courses>
        <tutors>
            <tutor id="1" name="Jane"/>
            <tutor id="2" name="Jake"/>
        </tutors>
      </department>

    println($(department, \\(student(name == "John"))\courseAxis) map name)
    println($(department, \\(tutor(name == "Jane"))\courseAxis) map name)
    println($(department, \\(student(name == "John"))\tutorAxis) map name)
    println($(department, \\(tutor(name == "Jane"))\studentAxis) map name)

    println($(department, * \\+ *) map name)
  }
}