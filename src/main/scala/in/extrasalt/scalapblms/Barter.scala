package in.extrasalt.scalapblms

class AssertionStatement(string: String) {
  private val _wordList = string.split(" ")

  val itemA: String = _wordList(2)
  val itemACount: Double = _wordList(1).toDouble
  val itemB: String = _wordList(5)
  val itemBCount: Double = _wordList(4).toDouble

  def priceOfItemB(valuesMap: Map[String, Double]): Double = {
    (itemACount/itemBCount)*valuesMap(itemA)
  }
}

class QuestionStatement(string: String){
  private val _wordList = string.split(" ")

  val itemA: String = _wordList(1)
  val itemB: String = _wordList(3)
}

object Barter {
  def findRatio(expression: String, valuesMap: Map[String, Double]) = {
   val statement = new QuestionStatement(expression)

    (valuesMap(statement.itemB)/valuesMap(statement.itemA)).round.toInt

  }

  def createStockExchange(strings: List[String]) = {
    //Assume the first object on LHS costs 1000 amic

    def loop(strings: List[String], valuesMap: Map[String, Double]): Map[String, Double] ={
      if(strings.isEmpty) {
        valuesMap
      } else {
        val statement = new AssertionStatement(strings.head)
        val priceOfItemB = statement.priceOfItemB(valuesMap)
        val newValuesMap = valuesMap + (statement.itemB -> priceOfItemB)
        loop(strings.tail, newValuesMap)
      }
    }

    val InitialValuesMap = Map(strings.head.split(" ")(2) -> 1000.00)

    loop(strings, InitialValuesMap)

  }

  def parseInput(strings: List[String]) : List[Int] = {
    val (assertions, questions) = strings.partition(_.startsWith("!"))
    val valuesMap = createStockExchange(assertions)
    questions.map((x) => findRatio(x, valuesMap))
  }
}
