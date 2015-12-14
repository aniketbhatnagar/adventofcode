import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

/**
 * @author aniket
 */
class Puzzle3Tests extends FlatSpec with Matchers {

  import Puzzle3._

  "Coordinate" should "calculate up direction correctly" in {
    Coordinate(0, 0).up should be (Coordinate(0, -1))
  }

  it should "calculate down direction correctly" in {
    Coordinate(0, 0).down should be (Coordinate(0, 1))
  }

  it should "calculate left direction correctly" in {
    Coordinate(0, 0).left should be (Coordinate(-1, 0))
  }

  it should "calculate right direction correctly" in {
    Coordinate(0, 0).right should be (Coordinate(1, 0))
  }

  "Puzzle3" should "calculate houses receive at least one present for example 1 in part 1 correctly" in {
    calcUniqueCoordinatesPart1(">") should be (2)
  }

  it should "calculate houses receive at least one present for example 2 in part 1 correctly" in {
    calcUniqueCoordinatesPart1("^>v<") should be (4)
  }

  it should "calculate houses receive at least one present for example 3 in part 1 correctly" in {
    calcUniqueCoordinatesPart1("^v^v^v^v^v") should be (2)
  }

  it should "solve part 1 correctly" in {
    calcUniqueCoordinatesPart1(input) should be (2592)
  }

  it should "calculate houses receive at least one present for example 1 in part 2 correctly" in {
    calcUniqueCoordinatesPart2("^v") should be (3)
  }

  it should "calculate houses receive at least one present for example 2 in part 2 correctly" in {
    calcUniqueCoordinatesPart2("^>v<") should be (3)
  }

  it should "calculate houses receive at least one present for example 3 in part 2 correctly" in {
    calcUniqueCoordinatesPart2("^v^v^v^v^v") should be (11)
  }

  it should "solve part 2 correctly" in {
    calcUniqueCoordinatesPart2(input) should be (2360)
  }

  private val input: String = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day3_input"))
    source.mkString
  }

}
