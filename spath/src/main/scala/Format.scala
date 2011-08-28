
import java.io.{FileReader, BufferedReader, FileInputStream}



/**
 * Created by IntelliJ IDEA.
 * User: nic
 * Date: 09-Oct-2010
 * Time: 11:38:12
 * To change this template use File | Settings | File Templates.
 */

object Format {

  def main(args: Array[String]): Unit = {

    val no = true
    //val r = new BufferedReader(new FileReader("C:\\Users\\nic\\IdeaProjects\\spath\\src\\main\\scala\\txt\\query"))
    val r = new BufferedReader(new FileReader("D:\\work\\github\\SPath\\spath\\src\\main\\scala\\examples\\EmployeesExample.scala"))
    //val r = new BufferedReader(new FileReader("C:\\Users\\nic\\IdeaProjects\\spath\\src\\main\\scala\\spath\\SPathLite.scala"))
    val start = 1
    var line : String  = null
    var count = 1

    line = r.readLine

    while (line != null) {

      line = line.replaceAll("\\\\","\\\\textbackslash")
      line = line.replace("$","\\$")
      line = line.replace("~","$\\sim$")
      line = line.replaceAll("\\{","\\\\{")
      line = line.replaceAll("\\}","\\\\}")
      line = line.replaceAll(">","\\$>\\$")
      line = line.replaceAll("<","\\$<\\$")
      line = line.replaceAll("\\*","\\\\textasteriskcentered")
      line = line.replaceAll("\\_","\\\\_")
      line = line.replaceAll("&","\\\\&")
      line = line.replaceAll("$","\\\\\\\\")
      line = line.replaceAll("\\\\textbackslash","{\\\\textbackslash}")



      for (i <- 30 to (1,-1)) {

        val reg = "\\A"+indent(i)
        line = line.replaceAll(reg, bol(i) +" ")
      }
      line = line.replaceAll("\\A", "&")

      if (line == "&\\\\")
        println("\\vspace{-3mm}"+line )
      else {

        if (no)
          println("\\lineno{"+ (count-start) +"}" + line )
        else
          println(line)

        count = count + 1
      }
      line = r.readLine
    }
  }

  def indent(i: Int) : String = if (i ==0) "" else "  " + indent(i-1)
  def bol(i : Int) : String = if (i == 0) "" else  bol(i-1) + "\\\\iind"

}
