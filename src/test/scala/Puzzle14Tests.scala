import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

/**
 * @author aniket
 */
class Puzzle14Tests extends FlatSpec with Matchers {

  import Puzzle14._
  import InputParser._

  "Reindeer" should "travel 1120 kms after 1000 seconds at speed 14 km/sec with fly time = 10 and rest time = 127" in {
    Reindeer("Comet", 14, 10, 127).distanceAfter(1000) should be (1120)
  }

  it should "travel 1056 kms after 1000 seconds at speed 16 km/sec with fly time = 11 and rest time = 162" in {
    Reindeer("Dancer", 16, 11, 162).distanceAfter(1000) should be (1056)
  }

  "InputParser" should "parse Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds." in {
    val reindeers = parse("Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.")
    reindeers.size should be (1)
    reindeers(0) should be (Reindeer("Comet", 14, 10, 127))
  }

  "Puzzle14" should "return 1120 as distance that the winning reindeer has traveled in example 1 in part 1" in {
    val reindeers = parse("""Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
                            |Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.""".stripMargin)
    winningDistanceAfter(reindeers, 1000) should be (1120)
  }

  it should "solve in part 1" in {
    val reindeers = parse(input)
    winningDistanceAfter(reindeers, 2503) should be (2640)
  }

  it should "calculate pointsAfter in example 1 in part 2" in {
    val reindeers = parse("""Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
                            |Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.""".stripMargin)
    pointsAfter(reindeers, 1000) should be (List(312, 689))
  }

  it should "solve in part 2" in {
    val reindeers = parse(input)
    winningPointsAfter(reindeers, 2503) should be (1102)
  }

  private val input: String = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day14_input"))
    source.mkString
  }
}
