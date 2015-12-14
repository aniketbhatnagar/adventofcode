/**
 * --- Day 3: Perfectly Spherical Houses in a Vacuum ---
 *
 * http://adventofcode.com/day/3
 *
 * @author aniket
 */
object Puzzle3 {

  def calcUniqueCoordinatesPart1(directions: String): Int = {
    val finalSanta = directions.foldLeft(Santa())((santa, direction) => santa.move(direction))
    finalSanta.coordinateHistory.distinct.size
  }

  def calcUniqueCoordinatesPart2(directions: String): Int = {
    val santas = List(Santa(), Santa())
    val zippedDirections = directions.zip(0 until directions.size)
    val finalSantas = zippedDirections.foldLeft(santas)((santas, directionWithIndex) => {
      val (direction, directionIndex) = directionWithIndex
      val santasIndex = directionIndex % santas.size
      santas.updated(santasIndex, santas(santasIndex).move(direction))
    })
    finalSantas.flatMap(_.coordinateHistory).distinct.size
  }

  case class Santa(coordinateHistory: List[Coordinate] = List(Coordinate(0, 0))) {
    def move(direction: Char): Santa = {
      val lastCoordinate = coordinateHistory.last
      val newCoordinate = direction match {
        case '^' => lastCoordinate.up
        case 'v' => lastCoordinate.down
        case '<' => lastCoordinate.left
        case '>' => lastCoordinate.right
      }
      Santa(coordinateHistory :+ newCoordinate)
    }
  }

  case class Coordinate(x: Int, y: Int) {
    def up: Coordinate = Coordinate(x, y - 1)
    def down: Coordinate = Coordinate(x, y + 1)
    def left: Coordinate = Coordinate(x - 1, y)
    def right: Coordinate = Coordinate(x + 1, y)
  }

}
