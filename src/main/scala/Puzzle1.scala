/**
 * --- Day 1: Not Quite Lisp ---
 *
 * http://adventofcode.com/day/1
 *
 * @author aniket
 */
object Puzzle1 {

  def calcFinalFloor(brackets: String): Int = {
    zipWithFloors(brackets).lastOption.map(_._2).getOrElse(0)
  }

  def calcFirstBasementIndex(brackets: String): Int = {
    val bracketsWithFloors = zipWithFloors(brackets)
    val floors = bracketsWithFloors.map(_._2)
    val floorsWithIndex = floors.zip(1 to brackets.size)
    val indexForBasement = floorsWithIndex.find {
      case (floor, index) => floor.equals(-1)
    }
    indexForBasement.get._2
  }

  private def zipWithFloors(brackets: String): List[(Char, Int)] = {
    val floors = brackets.foldLeft(List[Int]())((floorsList, bracket) => {
      val currentFloor = floorsList.lastOption.getOrElse(0)
      val newFloor = calcNewFloor(currentFloor, bracket)
      floorsList :+ newFloor
    })
    brackets.zip(floors).toList
  }

  private def calcNewFloor(currentFloor: Int, bracket: Char): Int = {
    bracket match {
      case '(' => currentFloor + 1
      case ')' => currentFloor - 1
    }
  }

}
