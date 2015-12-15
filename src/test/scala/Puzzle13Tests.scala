import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

/**
 * @author aniket
 */
class Puzzle13Tests extends FlatSpec with Matchers {
  import Puzzle13._
  import InputParser._

  "InputParser" should "parse Alice would gain 54 happiness units by sitting next to Bob." in {
    parse("Alice would gain 54 happiness units by sitting next to Bob.") should be(List(HappinessRule("Alice", "Bob", 54)))
  }

  it should "parse Alice would lose 79 happiness units by sitting next to Carol." in {
    parse("Alice would lose 79 happiness units by sitting next to Carol.") should be(List(HappinessRule("Alice", "Carol", -79)))
  }

  "Puzzle13" should "solve example in part 1" in {
    val rules = parse("""Alice would gain 54 happiness units by sitting next to Bob.
            |Alice would lose 79 happiness units by sitting next to Carol.
            |Alice would lose 2 happiness units by sitting next to David.
            |Bob would gain 83 happiness units by sitting next to Alice.
            |Bob would lose 7 happiness units by sitting next to Carol.
            |Bob would lose 63 happiness units by sitting next to David.
            |Carol would lose 62 happiness units by sitting next to Alice.
            |Carol would gain 60 happiness units by sitting next to Bob.
            |Carol would gain 55 happiness units by sitting next to David.
            |David would gain 46 happiness units by sitting next to Alice.
            |David would lose 7 happiness units by sitting next to Bob.
            |David would gain 41 happiness units by sitting next to Carol.""".stripMargin)
    maximizeHappiness(rules) should be (330)
  }

  it should "solve part 1" in {
    val rules = parse(input)
    maximizeHappiness(rules) should be (618)
  }

  it should "solve part 2" in {
    val rules = parse(input)
    maximizeHappinessIncludingMyself(rules) should be (601)
  }

  private val input: String = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day13_input"))
    source.mkString
  }
}
