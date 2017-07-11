package in.extrasalt.scalapblms

class AssertionStatement(string: String) {
  private val _wordList = string.split(" ")

  val itemA: String      = _wordList(2)
  val itemACount: Double = _wordList(1).toDouble
  val itemB: String      = _wordList(5)
  val itemBCount: Double = _wordList(4).toDouble

  def priceOfItemB(valuesMap: Map[String, Double]): Double = {
    (itemACount / itemBCount) * valuesMap(itemA)
  }
}

class QuestionStatement(string: String) {
  private val _wordList = string.split(" ")

  val itemA: String = _wordList(1)
  val itemB: String = _wordList(3)

  def findRatio(valuesMap: Map[String, Double]): Int = {
    (valuesMap(itemB) / valuesMap(itemA)).round.toInt
  }
}

object Barter {
  def createStockExchange(strings: List[String]): Map[String, Double] = {
    //Assume the first object on LHS costs 1000 amic
    def loop(strings: List[String], valuesMap: Map[String, Double]): Map[String, Double] = {
      if (strings.isEmpty) valuesMap
      else
        loop(strings.tail, {
          val statement = new AssertionStatement(strings.head)
          valuesMap + (statement.itemB -> statement.priceOfItemB(valuesMap))
        })
    }
    loop(strings, Map(new AssertionStatement(strings.head).itemA -> 1000.00))
  }

  def parseInput(strings: List[String]): List[Int] = {
    val (assertions, questions) = strings.partition(_.startsWith("!"))
    val valuesMap               = createStockExchange(assertions)
    questions.map((x) => new QuestionStatement(x).findRatio(valuesMap))
  }
}
