package in.extrasalt.scalapblms

object Barter {
  def createStockExchange(strings: List[String]) = {
    //Assume the first object on LHS costs 1000 amic

    def loop(strings: List[String], valuesMap: Map[String, Double]): Map[String, Double] ={
      if(strings.isEmpty) {
        valuesMap
      } else {
        val wordList = strings.head.split(" ")
        val itemB = wordList(5)
        val priceOfItemB = (wordList(1).toDouble / wordList(4).toDouble) * valuesMap(wordList(2))
        val newValuesMap = valuesMap + (itemB -> priceOfItemB)
        loop(strings.tail, newValuesMap)
      }
    }

    val InitialValuesMap = Map(strings.head.split(" ")(2) -> 1000.00)

    loop(strings, InitialValuesMap)

  }
}
