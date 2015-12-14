import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

/**
 * @author aniket
 */
class Puzzle12Tests extends FlatSpec with Matchers {
  import Puzzle12._
  import JsonParser._

  "JsonParser" should "parse int value" in {
    parse("1") should be (IntValue(1))
  }

  it should "parse string value" in {
    parse("\"abc\"") should be (StringValue("abc"))
  }

  it should "parse int array" in {
    parse("[1,2,3]") should be (JsonArray(List(IntValue(1), IntValue(2), IntValue(3))))
  }

  it should "parse simple json object" in {
    parse("""{"tesStringKey": "testValue", "testIntKey": 1}""") should be (
      JsonObject(List(
        ("tesStringKey", StringValue("testValue")),
        ("testIntKey", IntValue(1))
      ))
    )
  }

  "Puzzle12" should "sum in values in [1,2,3] as 6" in {
    sumIntValuesPart1("[1, 2, 3]") should be (6)
  }

  it should "sum in values in {\"a\":2,\"b\":4} as 6" in {
    sumIntValuesPart1("""{"a":2,"b":4}""") should be (6)
  }

  it should "sum in values in [[[3]]] as 3" in {
    sumIntValuesPart1("[[[3]]]") should be (3)
  }

  it should "sum in values in {\"a\":{\"b\":4},\"c\":-1} as 3" in {
    sumIntValuesPart1("""{"a":{"b":4},"c":-1}""") should be (3)
  }

  it should "sum in values in {\"a\":[-1,1]} as 0" in {
    sumIntValuesPart1("""{"a":[-1,1]}""") should be (0)
  }

  it should "sum in values in [-1,{\"a\":1}] as 0" in {
    sumIntValuesPart1("""[-1,{"a":1}]""") should be (0)
  }

  it should "sum in values in [] as 0" in {
    sumIntValuesPart1("[]") should be (0)
  }

  it should "sum in values in {} as 0" in {
    sumIntValuesPart1("{}") should be (0)
  }

  it should "solve part 1" in {
    sumIntValuesPart1(input) should be (156366)
  }

  it should "ignore json object because it contains red value in part 2" in {
    sumIntValuesPart2("""{"key1":1,"key2":"red"}""") should be (0)
  }

  it should "solve part 2" in {
    sumIntValuesPart2(input) should be (96852)
  }

  private val input: String = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day12_input"))
    source.mkString
  }
}
