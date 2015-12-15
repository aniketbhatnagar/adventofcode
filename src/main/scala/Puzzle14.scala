import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

/**
 * --- Day 14: Reindeer Olympics ---
 *
 * http://adventofcode.com/day/14
 *
 * @author aniket
 */
object Puzzle14 {
  case class Reindeer(name: String, speed: Int, flyTime: Int, restTime: Int) {
    def distanceAfter(nSeconds: Int): Int = {
      val cycleTime = flyTime + restTime
      val lastCycleTime = nSeconds % cycleTime
      val lastCycleFlyTime = Math.min(flyTime, lastCycleTime)
      (nSeconds / cycleTime) * speed * flyTime + speed * lastCycleFlyTime
    }
  }

  def winningDistanceAfter(reindeers: List[Reindeer], nSeconds: Int): Int = {
    reindeers.map(_.distanceAfter(nSeconds)).max
  }

  def winningPointsAfter(reindeers: List[Reindeer], nSeconds: Int): Int = {
    pointsAfter(reindeers, nSeconds).max
  }

  def pointsAfter(reindeers: List[Reindeer], nSeconds: Int): List[Int] = {
    (1 to nSeconds).foldLeft(List.fill(reindeers.size)(0))((points, second) => {
      val newPoints = pointsAt(reindeers, second)
      (points zip newPoints).map(oldAndNewPoints => oldAndNewPoints._1 + oldAndNewPoints._2)
    })
  }

  def pointsAt(reindeers: List[Reindeer], nSeconds: Int): List[Int] = {
    val distances = reindeers.map(_.distanceAfter(nSeconds))
    val maxDistance = distances.max
    distances.map(distance => {
      if (distance.equals(maxDistance)) {
        1
      } else {
        0
      }
    })
  }

  object InputParser extends JavaTokenParsers {
    def reindeer: Parser[Reindeer] = ((ident <~ "can fly")
                                        ~ wholeNumber
                                        ~ ("km/s for" ~> wholeNumber <~ "seconds, but then must rest for")
                                        ~ (wholeNumber <~ "seconds.")
                                     ).map(matched => {
                                       Reindeer(matched._1._1._1, matched._1._1._2.toInt, matched._1._2.toInt, matched._2.toInt)
                                     })
    def reindeers: Parser[List[Reindeer]] = reindeer*

    def parse(inputStr: String): List[Reindeer] = {
      val phraseParser = phrase(reindeers)
      val input = new CharSequenceReader(inputStr)
      phraseParser(input) match {
        case Success(reindeers, _) => reindeers
        case NoSuccess(msg, _) => {
          throw new IllegalArgumentException(s"Error parsing string $inputStr: $msg")
        }
      }
    }
  }
}
