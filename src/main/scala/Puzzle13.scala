import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

/**
 * --- Day 13: Knights of the Dinner Table ---
 *
 * http://adventofcode.com/day/13
 *
 * @author aniket
 */
object Puzzle13 {

  case class HappinessRule(attendee: String, neighbour: String, deltaPoints: Int)

  case class RoundTable(attendees: Array[String], happinessIndex: Map[(String, String), Int]) {
    lazy val totalHappiness: Int = {
      (0 until attendees.size).map(i => {
        val attendee = attendees(i)
        val neighbour = attendees((i + 1) % attendees.size)
        happinessIndex((attendee, neighbour)) + happinessIndex((neighbour, attendee))
      }).sum
    }
  }

  def maximizeHappinessIncludingMyself(rules: List[HappinessRule]): Int = {
    val attendees = rules.map(_.attendee).toSet.toList
    val meRules = attendees.flatMap(attendee => List(HappinessRule("me", attendee, 0), HappinessRule(attendee, "me", 0)))
    maximizeHappiness(rules ++ meRules)
  }

  def maximizeHappiness(rules: List[HappinessRule]): Int = {
    val happinessIndex = rules.map(rule => ((rule.attendee, rule.neighbour), rule.deltaPoints)).toMap
    val attendees = Array(rules.map(_.attendee).toSet.toList: _*)
    val possibleRoundTables = attendees.permutations.map(attendees => RoundTable(attendees, happinessIndex))
    possibleRoundTables.map(_.totalHappiness).max
  }

  object InputParser extends JavaTokenParsers {
    def name: Parser[String] = ident
    def sign: Parser[Int] = ("gain" | "lose").map(matched => matched match {
      case "gain" => 1
      case "lose" => -1
    })
    def points: Parser[Int] = wholeNumber.map(_.toInt)
    def rule: Parser[HappinessRule] = ((name <~ "would") ~ sign ~ (points <~ "happiness units by sitting next to") ~ (name <~ ".")).map(matched => {
      val deltaPoints = matched._1._2 * matched._1._1._2
      HappinessRule(matched._1._1._1, matched._2, deltaPoints)
    })
    def rules: Parser[List[HappinessRule]] = rule*

    def parse(rulesStr: String): List[HappinessRule] = {
      val phraseParser = phrase(rules)
      val input = new CharSequenceReader(rulesStr)
      phraseParser(input) match {
        case Success(rules, _) => rules
        case NoSuccess(msg, _) => {
          throw new IllegalArgumentException(s"Error parsing string $rulesStr: $msg")
        }
      }
    }

  }
}
