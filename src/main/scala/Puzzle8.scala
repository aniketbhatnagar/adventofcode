import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

/**
 * --- Day 8: Matchsticks ---
 *
 * http://adventofcode.com/day/8
 *
 * @author aniket
 */
object Puzzle8 {

  case class MatchedChar(char: Char, numCodeChars: Int, numMemoryChars: Int, numEncodedChars: Int)
  case class MatchedString(chars: List[MatchedChar]) {
    val numCodeChars = chars.map(char => char.numCodeChars).sum
    val numMemChars = chars.map(char => char.numMemoryChars).sum
    val numEncodedChars = chars.map(char => char.numEncodedChars).sum
    val str = new String(chars.map(char => char.char).toArray)
  }

  object StringParsers extends JavaTokenParsers {
    private val HEXA_RADIX = 16
    private val NUM_CODE_CHARS_ESCAPED_SLASH = 2
    private val NUM_CODE_CHARS_ESCAPED_QUOTE = 2
    private val NUM_CODE_CHARS_ESCAPED_CHAR = 4
    private val NUM_ENCODED_CHARS_SLASH = 4
    private val NUM_ENCODED_CHARS_QUOTE = 4
    private val NUM_ENCODED_CHARS_ESCAPED_CHAR = 5
    private val MATCHED_CHAR_UNESCAPED_QUOTE = MatchedChar('"', 1, 0, 3)

    def hexChar: Parser[Char] = toCharParser("[a-fA-F0-9]".r)
    def unescapedChar: Parser[MatchedChar] = toCharParser("[a-zA-Z]".r).map(char => MatchedChar(char, 1, 1, 1))
    def escapeChar: Parser[Char] = '\\'
    def escapedSlash: Parser[MatchedChar] = escapeChar ~> '\\'.map(char => MatchedChar(char, NUM_CODE_CHARS_ESCAPED_SLASH, 1, NUM_ENCODED_CHARS_SLASH))
    def escapedQuote: Parser[MatchedChar] = escapeChar ~> '"'.map(char => MatchedChar(char, NUM_CODE_CHARS_ESCAPED_QUOTE, 1, NUM_ENCODED_CHARS_QUOTE))
    def escapedHexaChar: Parser[MatchedChar] = (escapeChar ~> 'x' ~> repN(2, hexChar)).map(matchedChars => {
      val matchedStr = new String(matchedChars.toArray)
      val char = Integer.parseInt(matchedStr, HEXA_RADIX).toChar
      MatchedChar(char, NUM_CODE_CHARS_ESCAPED_CHAR, 1, NUM_ENCODED_CHARS_ESCAPED_CHAR)
    })
    def char: Parser[MatchedChar] = escapedHexaChar | escapedQuote | escapedSlash | unescapedChar
    def chars: Parser[List[MatchedChar]] = char*
    def nonEmptyStr: Parser[List[MatchedChar]] = ((('"' ~> chars) <~ '"')).map(matchedChars => {
      MATCHED_CHAR_UNESCAPED_QUOTE +: matchedChars :+ MATCHED_CHAR_UNESCAPED_QUOTE
    })
    def str: Parser[MatchedString] = ( nonEmptyStr).map(matchedChars => MatchedString(matchedChars))

    def parseStr(string: String): MatchedString = {
      val phraseParser = phrase(str)
      val input = new CharSequenceReader(string)
      phraseParser(input) match {
        case Success(matchedString, _) => matchedString
        case NoSuccess(msg, _) => {
          throw new IllegalArgumentException(s"Error parsing string $string: $msg")
        }
      }
    }
    private def toCharParser(parser: Parser[String]): Parser[Char] = {
      parser.map(matchedStr => {
        require(matchedStr.size == 1)
        matchedStr.charAt(0)
      })
    }

  }

}
