package org

import scala.swing.{Button, Alignment, TextField, Label, ComboBox}
import scala.language.implicitConversions
import javax.swing.border.CompoundBorder
import scala.swing.ListView.Renderer
import javax.swing.BorderFactory
import java.awt.Color._
import java.awt.Color

package object agh {

  // implicit conversions
  implicit def CellToValue(c: Cell): Color = c.value

  implicit def CellToCoordinate(c: Cell): (Int, Int) = (c.x, c.y)

  implicit def TextFieldLabelDouble(f: TextField): Double = f.text.toDouble

  implicit def IntLabelTextField(i: Int): TextField = textField(i)

  implicit def TextFieldToInt(tf: TextField): Int = tf.text.toInt

  implicit def StringToLabel(s: String): Label = new Label(s)

  implicit def StringToBorder(s: String): CompoundBorder = {
    BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder(s),
      BorderFactory.createEmptyBorder(5, 5, 5, 5))
  }

  implicit def SeqNeighboursToComboBox(sq: Seq[Neighbourhood.Value]): ComboBox[Neighbourhood.Value] = {
    new ComboBox(sq) {
      renderer = Renderer(_.toString)
    }
  }

  implicit def SeqBoundariesToComboBox(sq: Seq[Boundaries.Value]): ComboBox[Boundaries.Value] = {
    new ComboBox(sq) {
      renderer = Renderer(_.toString)
    }
  }

  implicit def StringToButton(s: String): Button = new Button(s)

  // random
  val RANDOM = new scala.util.Random()

  def randomCase = (cases: Int) => RANDOM.nextInt(cases) + 1

  def randomFloat = RANDOM.nextFloat()

  def randomInt = (max: Int) => RANDOM.nextInt(max)

  def randomCell(implicit cells: Seq[Cell]) = RANDOM.shuffle(cells).head

  def randomBoolean = RANDOM.nextBoolean()

  def randomColor = getHSBColor(randomFloat, 1f, 1f)

  // view helpers
  def textField(d: Double) = new TextField {
    text = d.toString
    columns = 5
    horizontalAlignment = Alignment.Left
  }

}
