package examples

import _root_.xspath.XSPathLite
import xml.Node

object ManyToManyRelationExample extends XSPathLite {

  def student  = elem("student")
  def students = elem("students")
  def student(p: Predicate): Expr = p and student

  def courses = elem("courses")
  def course = elem("course")
  def course(p: Predicate): Expr = p and course

  def tutor = elem("tutor")
  def tutors = elem("tutors")
  def tutor(p: Predicate): Expr = p and tutor

  def id(txt: String) = @@("id", txt)
  def tid(txt: String) = @@("tid", txt)
  def name(txt: String) = @@("name", txt)

  def courseAxis : axis = n => n.label match {
    case "student" =>
      $(n, \(parent)\(parent)\(courses)\(course(exists(\(students)\(student(id(@@("id", n))))))))
    case "tutor" =>
      $(n, \(parent)\(parent)\(courses)\(course(tid(@@("id",n)))))
    case _ => empty
  }

  def tutorAxis : axis = n => n.label match {
    case "student" => $(n, \(courseAxis)\(tutorAxis))
    case "course" => $(n, \(parent)\(parent)\(tutors)\(tutor(id(@@("tid", n)))))
    case _ => empty
  }

  def studentAxis : axis = n => n.label match {
    case "tutor" => $(n, \(courseAxis)\(studentAxis))
    case "course" => $(n, \(parent)
                          \(parent)
                          \(students)
                          \(student(@@("id",
                                        \(parent)
                                        \(parent)
                                        \(courses)
                                        \(course(id(@@("id",n))))
                                        \(students)
                                        \(student)))))
    case _ => empty
  }

  def main(args: Array[String]): Unit = {
    val department =
      <departement>
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
      </departement>

    println($(department, \\(student(name("John")))\courseAxis) map @@("name"))
    println($(department, \\(tutor(name("Jane")))\courseAxis) map @@("name"))
    println($(department, \\(student(name("John")))\tutorAxis) map @@("name"))
    println($(department, \\(tutor(name("Jane")))\studentAxis) map @@("name"))
  }
}