package in.extrasalt.scalapblms

class CurrencyExchange(names: Map[String, String]) {
  def eval(question: String): String = {
    val units = names.keySet
    question.split(" ").toList.filter((x) => units.contains(x)).map((x) => names(x)).mkString("")
  }

}

object CurrencyExchange {
  def convert(roman : String): Int = {
//    "I" can be subtracted from "V" and "X" only.
//    "X" can be subtracted from "L" and "C" only.
//    "C" can be subtracted from "D" and "M" only.

    val valueMap: Map[String, Int] = Map(
      "IV" -> 4,
      "IX" -> 9,
      "XL" -> 40,
      "XC" -> 90,
      "CD" -> 400,
      "CM" -> 900,
      "I" -> 1,
      "V" -> 5,
      "X" -> 10,
      "L" ->  50,
      "C" -> 100,
      "D" -> 500,
      "M" -> 1000
    )

    def loop(roman: List[Char], acc: Int): Int = roman match {
      case Nil => acc
      case a::b::tail if valueMap.keySet(a.toString+b.toString) => loop(tail, acc+valueMap(a.toString+b.toString))
      case a::tail => loop(tail, acc+valueMap(a.toString))

    }

    loop(roman.toList, 0)

  }

  def parse(statement: String, dictionary: Map[String, Char]): String = {
    val metricSet = dictionary.keySet
    val thingSet = Set("Silver", "Gold", "Iron")
    val romanValue = statement.split(" ").filter(metricSet).map((x) => dictionary(x)).toString

    val thing = statement.split(" ").filter(thingSet)

    if(romanValue.isEmpty || thing.isEmpty) return "I have no idea what you are talking about"
    //TODO: Multiply by value of the thing
    convert(romanValue).toString
  }
}
