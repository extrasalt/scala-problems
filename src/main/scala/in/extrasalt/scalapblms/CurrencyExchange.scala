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

    def loop(roman: List[Char], acc: Int): Int = roman match {
      case Nil => acc
      case 'I'::'V'::tail => loop(tail,acc+4)
      case 'I'::'X'::tail => loop(tail, acc+9)
      case 'X'::'L'::tail => loop(tail, acc+40)
      case 'X'::'C'::tail => loop(tail, acc+90)
      case 'C'::'D'::tail => loop(tail, acc+400)
      case 'C'::'M'::tail => loop(tail, acc+900)
      case 'I'::tail => loop(tail,acc+1)
      case 'V'::tail => loop(tail,acc+5)
      case 'X'::tail => loop(tail,acc+10)
      case 'L'::tail => loop(tail,acc+50)
      case 'C'::tail => loop(tail,acc+100)
      case 'D'::tail => loop(tail,acc+500)
      case 'M'::tail => loop(tail,acc+1000)

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
