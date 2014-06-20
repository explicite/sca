package org

import java.awt.Color
import java.awt.Color._
import javax.swing.BorderFactory
import javax.swing.border.CompoundBorder

import scala.language.implicitConversions
import scala.swing.ListView.Renderer
import scala.swing.{Alignment, Button, ComboBox, Label, TextField}

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

  implicit def SeqToComboBox[A](sq: Seq[(String, A)]): ComboBox[(String, A)] = {
    new ComboBox(sq) {
      renderer = Renderer(_._1)
    }
  }

  implicit def ComboBoxToValue[A](cb: ComboBox[(String, A)]): A = cb.selection.item._2

  implicit def ToupleToVal[A](t: (String, A)): A = t._2

  implicit def StringToButton(s: String): Button = new Button(s)

  // random
  val RANDOM = new scala.util.Random()

  def randomCase = (cases: Int) => RANDOM.nextInt(cases) + 1

  def randomFloat = RANDOM.nextFloat()

  def randomDouble = (max: Double) => RANDOM.nextDouble() * max

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

  def swap(cell: Cell)(space: scala.collection.mutable.Seq[Cell]) = {
    val index = space.indexWhere(c => cell.x == c.x && cell.y == c.y)
    space(index) = cell
  }

  def update(cells: Seq[Cell])(implicit space: Seq[Cell]): Seq[Cell] = {
    val mspace = scala.collection.mutable.Seq[Cell](space.seq: _*)
    cells.par.foreach {
      cell => swap(cell)(mspace)
    }

    mspace
  }
}
