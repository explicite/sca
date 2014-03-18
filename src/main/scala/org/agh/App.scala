package org.agh
import scala.swing
import javax.swing.UIManager

/**
 * @author Jan Paw 
 *         Date: 3/18/14
 */
object App extends SwingApplication {


  def top = new MainFrame {

  }
  override def startup(args: Array[String]) {
    UIManager.setLookAndFeel(
      UIManager.getSystemLookAndFeelClassName)
    top.visible = true
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(util.Properties.userDir, path)
}
