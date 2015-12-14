import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

/**
 * @author aniket
 */
class Puzzle1Tests extends FlatSpec with Matchers {

  import Puzzle1._

  "Puzzle1" should "return 0 in case of balanced brackets return 0 (effectively pass example 1)in part 1" in {
    calcFinalFloor("") should be (0)
    calcFinalFloor("(())") should be (0)
    calcFinalFloor("()()") should be (0)
  }

  it should "pass example 2 in part 1" in {
    calcFinalFloor("(((") should be (3)
    calcFinalFloor("(()(()(") should be (3)
  }

  it should "pass example 3 in part 1" in {
    calcFinalFloor("))(((((") should be (3)
  }

  it should "pass example 4 in part 1" in {
    calcFinalFloor("())") should be (-1)
    calcFinalFloor("))(") should be (-1)
  }

  it should "pass example 5 in part 1" in {
    calcFinalFloor(")))") should be (-3)
    calcFinalFloor(")())())") should be (-3)
  }

  it should "solve the puzzle with provided input part 1" in {
    calcFinalFloor(input) should be (74)
  }

  it should "pass example 1 in part 2" in {
    calcFirstBasementIndex(")") should be (1)
  }

  it should "pass example 2 in part 2" in {
    calcFirstBasementIndex("()())") should be (5)
  }

  it should "solve the puzzle with provided input part 2" in {
    calcFirstBasementIndex(input) should be (1795)
  }

  private val input: String = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day1_input"))
    source.mkString
  }
}
