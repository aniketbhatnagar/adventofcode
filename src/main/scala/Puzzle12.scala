import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

/**
 * --- Day 12: JSAbacusFramework.io ---
 *
 * http://adventofcode.com/day/12
 *
 * @author aniket
 */
object Puzzle12 {

  sealed trait JsonElement
  sealed trait JsonValue extends JsonElement

  case class IntValue(value: Int) extends JsonValue
  case class StringValue(value: String) extends JsonValue

  case class JsonArray(children: List[JsonElement]) extends JsonElement
  case class JsonObject(children: List[(String, JsonElement)]) extends JsonElement

  def sumIntValuesPart1(jsonStr: String): Int = {
    val json = JsonParser.parse(jsonStr)
    sumIntValues(json, _ => true)
  }

  def sumIntValuesPart2(jsonStr: String): Int = {
    val json = JsonParser.parse(jsonStr)
    sumIntValues(json, redFilter)
  }

  private def redFilter(jsonObject: JsonObject): Boolean = {
    !jsonObject.children.map(_._2).exists(jsonElement => {
      jsonElement match {
        case StringValue(value) => value.equals("red")
        case _                  => false
      }
    })
  }

  private def sumIntValues(jsonElement: JsonElement, jsonObjectFilter: JsonObject => Boolean): Int = {
    jsonElement match {
      case StringValue(_) => 0
      case IntValue(value) => value
      case JsonArray(elements) => elements.map(element => sumIntValues(element, jsonObjectFilter)).sum
      case jsonObject: JsonObject => {
        if (jsonObjectFilter(jsonObject)) {
          jsonObject.children
            .map(elementWithKey => sumIntValues(elementWithKey._2, jsonObjectFilter))
            .sum
        } else {
          0
        }

      }
    }
  }

  object JsonParser extends JavaTokenParsers {
    def intValue: Parser[IntValue] = wholeNumber.map(numer => IntValue(numer.toInt))
    def stringValue: Parser[StringValue] = stringLiteral.map(matched => StringValue(matched.substring(1, matched.size - 1)))
    def emptyJsonArray: Parser[JsonArray] = ("[" ~ "]").map(_ => JsonArray(List()))
    def nonEmptyJsonArray: Parser[JsonArray] = ("[" ~> (jsonElement ~ rep("," ~> jsonElement)) <~ "]").map(matched => {
      JsonArray(matched._1 +: matched._2)
    })
    def jsonArray: Parser[JsonArray] = emptyJsonArray | nonEmptyJsonArray
    def emptyJsonObject: Parser[JsonObject] = ("{" ~ "}").map(_ => JsonObject(List()))
    def jsonKeyPair: Parser[(String, JsonElement)] = ((stringLiteral <~ ":") ~ jsonElement).map(matched => {
      (matched._1.substring(1, matched._1.size - 1), matched._2)
    })
    def nonEmptyJsonObject: Parser[JsonObject] = ("{" ~> (jsonKeyPair ~ rep("," ~> jsonKeyPair)) <~ "}").map(matched => {
      JsonObject(matched._1 +: matched._2)
    })
    def jsonObject: Parser[JsonObject] = emptyJsonObject | nonEmptyJsonObject
    def jsonElement: Parser[JsonElement] = intValue | stringValue | jsonArray | jsonObject

    def parse(jsonStr: String): JsonElement = {
      val phraseParser = phrase(jsonElement)
      val input = new CharSequenceReader(jsonStr)
      phraseParser(input) match {
        case Success(json, _) => json
        case NoSuccess(msg, _) => {
          throw new IllegalArgumentException(s"Error parsing string $jsonStr: $msg")
        }
      }
    }
  }


}
