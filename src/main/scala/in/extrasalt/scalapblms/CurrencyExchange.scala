package in.extrasalt.scalapblms

class CurrencyExchange(names: Map[String, String]) {
  def eval(question: String): String = {
    val units = names.keySet
    question.split(" ").toList.filter((x) => units.contains(x)).map((x) => names(x)).mkString("")
  }

}
