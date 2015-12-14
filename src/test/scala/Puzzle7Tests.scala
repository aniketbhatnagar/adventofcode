import java.util.UUID
import java.util.concurrent.TimeUnit

import org.scalatest.{Matchers, FlatSpec}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

/**
 * @author aniket
 */
class Puzzle7Tests extends FlatSpec with Matchers {

  import Puzzle7._

  private val ASYNC_WAIT_SECS = 5

  implicit def valueToValueSource(value: Int) = new ValueSource(value.toChar)

  "And" should "return x when x AND x" in {
    And(0, 0).value should be (0)
    And(110, 110).value should be (110)
    And(65535, 65535).value should be (65535)
  }

  it should "return 2 when 3 AND 2" in {
    And(3, 2).value.toInt should be (2)
  }

  it should "return 1 when 5 AND 3" in {
    And(5, 3).value should be (1)
  }

  "Or" should "return x when x OR x" in {
    Or(0, 0).value should be (0)
    Or(110, 110).value should be (110)
    Or(65535, 65535).value should be (65535)
  }

  it should "return 7 when 5 OR 3" in {
    Or(5, 3).value should be (7)
  }

  "Not" should "return 65535 when NOT 0" in {
    Not(0).value should be (65535)
  }

  it should "return 0 when NOT 65535" in {
    Not(65535).value should be (0)
  }

  it should "return 20 when NOT 65515" in {
    Not(65515).value should be (20)
  }

  "LShift" should "return 2 when LSHIFT 1, 1" in {
    LShift(1, 1).value should be (2)
  }

  it should "return 65534 when LSHIFT 1, 65535" in {
    LShift(65535, 1).value should be (65534)
  }

  "RShift" should "return 0 when RSHIFT 1, 1" in {
    RShift(1, 1).value should be (0)
  }

  it should "return 65534 when RSHIFT 1, 65535" in {
    RShift(65535, 1).value should be (32767)
  }

  "InstructionParsers" should "parse Relay instruction with input value correctly" in {
    val wire = parse("123 -> x")
    wire.name should be ("x")
    wire.sourceOpt should be (Some(ValueSource(123)))
  }

  it should "parse Relay instruction with input wire correctly" in {
    val wire = parse("y -> x")
    wire.name should be ("x")
    wire.sourceOpt should be (Some(WireSource(Wire("y", None))))
  }

  it should "parse Not instruction with input wire correctly" in {
    val wire = parse("NOT x -> h")
    wire.name should be ("h")
    wire.sourceOpt should be (Some(GateSource(Not(WireSource(Wire("x", None))))))
  }

  it should "parse Not instruction with input value correctly" in {
    val wire = parse("NOT 1 -> h")
    wire.name should be ("h")
    wire.sourceOpt should be (Some(GateSource(Not(ValueSource(1)))))
  }

  it should "parse And instruction with input wires correctly" in {
    val wire = parse("x AND y -> d")
    wire.name should be ("d")
    wire.sourceOpt should be (Some(GateSource(And(WireSource(Wire("x", None)), WireSource(Wire("y", None))))))
  }

  it should "parse And instruction with input value and input wire correctly" in {
    val wire = parse("1 AND y -> d")
    wire.name should be ("d")
    wire.sourceOpt should be (Some(GateSource(And(ValueSource(1), WireSource(Wire("y", None))))))
  }

  it should "parse Or instruction with input wires correctly" in {
    val wire = parse("x OR y -> e")
    wire.name should be ("e")
    wire.sourceOpt should be (Some(GateSource(Or(WireSource(Wire("x", None)), WireSource(Wire("y", None))))))
  }

  it should "parse Or instruction with input wire and input value correctly" in {
    val wire = parse("1 OR y -> e")
    wire.name should be ("e")
    wire.sourceOpt should be (Some(GateSource(Or(ValueSource(1), WireSource(Wire("y", None))))))
  }

  it should "parse LShift instruction correctly" in {
    val wire = parse("x LSHIFT 2 -> f")
    wire.name should be ("f")
    wire.sourceOpt should be (Some(GateSource(LShift(WireSource(Wire("x", None)), 2))))
  }

  it should "parse RShift instruction correctly" in {
    val wire = parse("y RSHIFT 2 -> g")
    wire.name should be ("g")
    wire.sourceOpt should be (Some(GateSource(RShift(WireSource(Wire("y", None)), 2))))
  }

  "Puzzle7" should "solve example in part 1" in {
    val circuit = parseCircuit("""123 -> x
            |456 -> y
            |x AND y -> d
            |x OR y -> e
            |x LSHIFT 2 -> f
            |y RSHIFT 2 -> g
            |NOT x -> h
            |NOT y -> i""".stripMargin)
    circuit.valueOf("d") should be (72)
    circuit.valueOf("e") should be (507)
    circuit.valueOf("f") should be (492)
    circuit.valueOf("g") should be (114)
    circuit.valueOf("h") should be (65412)
    circuit.valueOf("i") should be (65079)
    circuit.valueOf("x") should be (123)
    circuit.valueOf("y") should be (456)
  }

  it should "solve sample relay circuit" in {
    val circuit = parseCircuit(
      """123 -> xy
        |xy -> ab
      """.stripMargin)
    circuit.valueOf("xy") should be (123)
    circuit.valueOf("ab") should be (123)
  }

  it should "solve multi-valued relay circuit" in {
    val circuit = parseCircuit(
      """123 -> xy
        |xy -> ab
        |456 -> xy
      """.stripMargin)
    circuit.valueOf("xy") should be (456)
    circuit.valueOf("ab") should be (456)
  }

  it should "solve part 1" in {
    val circuit = parseCircuit(input)
    circuit.valueOf("a").toInt should be (3176)
  }

  it should "solve part 2" in {
    val circuit = parseCircuit(input)
    val aValue = circuit.valueOf("a")
    circuit.overrideSource("b", ValueSource(aValue))
    circuit.valueOf("a").toInt should be (14710)
  }

  private def parse(instruction: String): Wire = {
    val parsers = new InstructionParsers()
    import parsers._
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(expr)
    //we need to wrap the expression string in a reader so our parser can digest it
    val input = new CharSequenceReader(instruction)
    phraseParser(input) match {
      case Success(wire, _) => wire
      case NoSuccess(msg, _) => {
        throw new IllegalArgumentException(s"Error parsing instruction $instruction: $msg")
      }
    }
  }

  private val input: String = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day7_input"))
    source.mkString
  }

}
