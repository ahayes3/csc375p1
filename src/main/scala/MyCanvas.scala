import java.awt.{Color, Graphics, Graphics2D, RenderingHints}
import javax.swing.JComponent

class MyCanvas extends JComponent{
  var factory:Factory = _
  override def paintComponent(g: Graphics): Unit = {
    if(g.isInstanceOf[Graphics2D]) {
      val g2 = g.asInstanceOf[Graphics2D]
      var hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON)
      g2.setRenderingHints(hints)


      //draw stuff
      if(factory == null) {
        g2.fillRect(0,0,100,100)
      }
      else {
        g2.setColor(Color.LIGHT_GRAY)
        g2.fillRect(0,0,factory.x*25,factory.y*25)
        g2.setColor(Color.DARK_GRAY)
        for(i <- 0 to factory.x) {
          g2.drawLine(i*25,0,i*25,factory.y*25)

        }
        for(i <- 0 to factory.y) {
          g2.drawLine(0,i*25,factory.x*25,i*25)
        }
        g2.setColor(Color.BLACK)
        for(i <- 0 until factory.x) {
          for(j <- 0 until factory.y) {
            if(factory.floor(i)(j).nonEmpty) {
              g2.drawString(factory.floor(i)(j).get.flavor.toString,(i*25)+12,(j*25)+12)
            }
          }
        }
      }

    }

  }
}
