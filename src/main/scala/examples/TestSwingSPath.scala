package examples

import _root_.swingspath.SwingSPath
import java.awt._
import runtime.Int
import javax.swing._
import swingspath.SwingSPath
import java.lang.{System}

object TestSwingSPath extends SwingSPath {
  def main(args: Array[String]): Unit = {

    SwingUtilities.invokeAndWait(new Runnable() {
      def run = {
        var f = new JFrame("")
        var cp = f.getContentPane

       // cp.setLayout(new GridLayout(2,2))
        val sp: TreeMapPanel = new TreeMapPanel(Color.WHITE, new Dimension(180, 180))
        fill(sp,1)
        cp add sp


        if (false) {

          val id = (o: Component) => scala.List(o)
          val f = * \\ (id, *)

          var firstResult = $(f)(cp)


        }
        else  if (false) {

          val query = \\(*)
          var result = $(query)(cp)
          for (o <- result) o match {case c: TreeMapPanel => c.color = Color.green case _ =>}


        } else  if (true) {

          val query = \(descendants(3), nth(1)\(following, nth(2)))
          var result = $(query)(cp)
          for (o <- result) o match {case c: TreeMapPanel => c.color = Color.green case _ =>}


        } else
        if (true) {
          val f = \(descendants(2))\ nth(1)\(following)

          var firstResult = $(f)(cp)

          for (o <- firstResult) o match {case c: TreeMapPanel => c.color = Color.red case _ =>}
        }

        else
        if (false) {
          val first = \\(child, nth(1))
          var firstResult = $(first)(cp)
          println(firstResult.size)
          for (o <- firstResult) o match {case c: TreeMapPanel => c.color = Color.green case _: JPanel =>}
        } else {

          val even = \\(descendants(2)) // => children(2)(a).size == 0)
          val odd = \(child) \\ descendants(2)


          var evenResult = $(even)(cp)
          var oddResult = $(odd)(cp)

          for (o <- evenResult) o match {case c: TreeMapPanel => c.color = blue case _: JPanel =>}
          for (o <- oddResult) o match {case c: TreeMapPanel => c.color = green case _: JPanel =>}
        }

        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        f.pack()
        f.setVisible(true)
        f.setLocationRelativeTo(null)

      }
    })
  }

  val divider = 2
  val gapPercentage = 0.05d

  val red = new Color(240, 215, 215)
  val blue = new Color(215, 215, 240)
  val green = new Color(235, 255, 235)

  var panels = 0
  def fill(currentPanel: TreeMapPanel, depth : Int): Unit = {

    val currentDimension: Dimension = currentPanel.getPreferredSize()
    var gap = (currentDimension.getWidth() * gapPercentage).intValue
    gap = Math.max(gap, 5)
    val nextDimension = new Dimension(
      ((currentDimension.getWidth() - ((divider + 1) * gap)) / 2).toInt,
      ((currentDimension.getHeight() - ((divider + 1) * gap)) / 2).toInt
      )

    if (nextDimension.width > 0) {
      var y = 0
      for (i <- 1 to divider * divider) {
        val nextPanel: TreeMapPanel = new TreeMapPanel(Color.WHITE, nextDimension)
        nextPanel.setName(depth + "_" + i)

        val c = new GridBagConstraints()

        c.gridx = (i+1) % divider
        c.gridy = y
        if (i % 2 == 0) y = y +1


        c.ipadx = gap
        c.ipady = gap

        val gapHalf = (gap /2).intValue
        var top = if (c.gridy > 0 && divider > 1) gapHalf else 0
        var right = if (c.gridx < divider -1 && divider > 1) gapHalf else 0
        var bottom = if (c.gridy < divider -1 && divider > 1) gapHalf else 0
        var left = if (c.gridx > 0 && divider > 1) gapHalf else 0

        bottom =  0
        left =  0

        c.insets = new Insets(top,left,bottom,right)

        fill(nextPanel, depth+1)
        currentPanel add(nextPanel, c)
        panels = panels + 1

      }
    }
  }
}

class TreeMapPanel(var color: Color, d: Dimension) extends JPanel {
  setPreferredSize(d)
  setLayout(new GridBagLayout())
  override def paintComponent(g: Graphics) = {

    g.setColor(color)

    val width = getSize().width
    val height = getSize().height
    val arcWidth = (width * 0.2d).toInt
    val arcHeight = (height * 0.2d).toInt

    g.fillRoundRect(0, 0, getSize().width, getSize().height, arcWidth, arcHeight)
    g.setColor(Color.black)

    g match {
      case g2: Graphics2D => g2.setStroke(new BasicStroke(3))
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    }

    g.drawRoundRect(0, 0, getSize().width, getSize().height, arcWidth, arcHeight)
  }
}