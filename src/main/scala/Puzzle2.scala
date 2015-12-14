/**
 * --- Day 2: I Was Told There Would Be No Math ---
 *
 * http://adventofcode.com/day/2
 *
 * @author aniket
 */
object Puzzle2 {

  def calcRequiredPaper(boxes: List[Box]): Long = {
    boxes.map(calcRequiredPaper(_)).sum
  }

  def calcRequiredPaper(box: Box): Long = {
    val minSurface = box.surfaces.map(_.surfaceArea).min
    box.surfaceArea + minSurface
  }

  def calcRequiredRibbon(boxes: List[Box]): Long = {
    boxes.map(calcRequiredRibbon(_)).sum
  }

  def calcRequiredRibbon(box: Box): Long = {
    box.surfaces.map(_.perimeter).min + box.volume
  }


  case class Box(length: Int, breadth: Int, height: Int) {
    lazy val dimensions: List[Int] = List(length, breadth, height)
    lazy val surfaces: List[Rectangle] = (0 until dimensions.size).combinations(2).flatMap(combination => {
      val rectangle = Rectangle(dimensions(combination(0)), dimensions(combination(1)))
      List(rectangle, rectangle) // Since each rectangle is present at opposite ends in the box.
    }).toList
    lazy val surfaceArea: Int = surfaces.map(_.surfaceArea).sum
    lazy val volume: Int = length * breadth * height
  }

  case class Rectangle(length: Int, height: Int) {
    lazy val surfaceArea: Int = length * height

    lazy val perimeter: Int = 2 * (length + height)
  }
}
