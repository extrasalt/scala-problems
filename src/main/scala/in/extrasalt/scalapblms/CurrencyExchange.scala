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
      case a::b::tail if a=='I' && b =='V' => loop(tail,acc+4)
      case a::b::tail if a=='I' && b == 'X' => loop(tail, acc+9)
      case a::b::tail if a=='X' && b == 'L' => loop(tail, acc+40)
      case a::b::tail if a=='X' && b == 'C' => loop(tail, acc+90)
      case a::b::tail if a=='C' && b == 'D' => loop(tail, acc+400)
      case a::b::tail if a=='C' && b == 'M' => loop(tail, acc+900)
      case a::tail if a=='I' => loop(tail,acc+1)
      case a::tail if a=='V' => loop(tail,acc+5)
      case a::tail if a=='X' => loop(tail,acc+10)
      case a::tail if a=='L' => loop(tail,acc+50)
      case a::tail if a=='C' => loop(tail,acc+100)
      case a::tail if a=='D' => loop(tail,acc+500)
      case a::tail if a=='M' => loop(tail,acc+1000)

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
