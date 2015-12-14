import scala.annotation.tailrec

/**
 * --- Day 11: Corporate Policy ---
 *
 * http://adventofcode.com/day/11
 *
 * @author aniket
 */
object Puzzle11 {

  sealed trait Error

  case object NoThreeIncreasingLetters extends Error
  case object NoIOLAllowed extends Error
  case object NoTwoPairs extends Error

  type Validator = String => Either[Error, Unit]

  def nextValidPassword(initPassword: String): String = {
    passwordsFrom(initPassword).find(password => validate(password).isRight).get
  }

  def passwordsFrom(init: String): Stream[String] = {
    Stream.iterate(genNextPassword(init))(lastPassword => genNextPassword(lastPassword))
  }

  def validate(str: String): Either[Error, Unit] = {
    validate(str, List(validate3StraightChars, validateNoIOLChars, validate2DifferentPairs))
  }

  private def genNextPassword(str: String): String = {
    @tailrec
    def genNextPassItr(str: String, index: Int): String = {
      if (index < 0) {
        "a" + str
      } else {
        val chatAtIndex = str.charAt(index)
        if (chatAtIndex < 'z') {
          new String(str.updated(index, (chatAtIndex + 1).toChar).toArray)
        } else {
          val newStr = new String(str.updated(index, 'a').toArray)
          genNextPassItr(newStr, index - 1)
        }
      }
    }
    genNextPassItr(str, str.size - 1)
  }

  private def validate(str: String, validators: List[Validator]): Either[Error, Unit] = {
    validators.tail.foldLeft(validators(0)(str))((prevResult, validator) => {
      prevResult.right.flatMap(_ => validator(str))
    })
  }

  def validate3StraightChars(str: String): Either[Error, Unit] = {
    val finalState = str.tail.tail.foldLeft((str(0), str(1), false))((state, char) => {
      val (secondLastChar, lastChar, result) = state
      result match {
        case false => {
          if (lastChar - secondLastChar == 1 && char - lastChar == 1) {
            (lastChar, char, true)
          } else {
            (lastChar, char, false)
          }
        }
        case true => (lastChar, char, result)
      }
    })
    if (finalState._3) {
      Right()
    } else {
      Left(NoThreeIncreasingLetters)
    }
  }

  def validate2DifferentPairs(str: String): Either[Error, Unit] = {
    val finalState = str.tail.foldLeft((str.head, Set[String]()))((state, char) => {
      val (lastChar, pairs) = state
      if (lastChar.equals(char)) {
        val updatePairs = pairs + new String(Array(lastChar, char))
        (char, updatePairs)
      } else {
        (char, pairs)
      }
    })
    if (finalState._2.size > 1) {
      Right()
    } else {
      Left(NoTwoPairs)
    }
  }

  def validateNoIOLChars(str: String): Either[Error, Unit] = {
    val blackistedChars = Set('i', 'o', 'l')
    if (str.find(char => blackistedChars.contains(char)).isDefined) {
      Left(NoIOLAllowed)
    } else {
      Right()
    }
  }
}
