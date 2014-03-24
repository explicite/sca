package org.agh


/**
 * @author Jan Paw
 *         Date: 3/16/14
 */
abstract case class Space(width: Int, height: Int) extends Neighbours {

  def iterate(space: Seq[Float]): Seq[Float] = {
    var tmp: Seq[Float] = Nil
    def cellValue(x: Int, y: Int): Float = {
      space(width * y + x) match {
        case 0 =>
          val values = neighbours(x, y).map(c => space(width * c._2 + c._1)).filter(x => x > 0)
          val valuesWithCount = values map (v => (v, values.count(_ == x)))
          val sorted = valuesWithCount.sortWith(_._2 > _._2)
          sorted match {
            case h :: tail => h._1
            case _ => space(width * y + x)
          }
        case _ => space(width * y + x)
      }
    }

    //Optimization
    var x: Int = 0
    var y: Int = 0

    while (y < width) {

      while (x < height) {
        tmp ++= cellValue(x, y) :: Nil
        x += 1

      }

      y += 1
      x = 0
    }

    tmp
  }
}
