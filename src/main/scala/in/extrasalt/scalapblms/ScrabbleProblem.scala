package in.extrasalt.scalapblms

object ScrabbleProblem {
  val defaultScoreMap: Map[Char, Int] = Map(
    'a' -> 1,
    'b' -> 3,
    'c' -> 3,
    'd' -> 2,
    'e' -> 1,
    'f' -> 4,
    'g' -> 2,
    'h' -> 4,
    'i' -> 1,
    'j' -> 8,
    'k' -> 5,
    'l' -> 1,
    'm' -> 3,
    'n' -> 1,
    'o' -> 1,
    'p' -> 3,
    'q' -> 10,
    'r' -> 1,
    's' -> 1,
    't' -> 1,
    'u' -> 1,
    'v' -> 4,
    'w' -> 4,
    'x' -> 8,
    'y' -> 4,
    'z' -> 10
  )

  val scrabbleBoard: List[List[String]] = List(
    List("TW", "NL", "NL", "DL", "NL", "NL", "NL", "TW", "NL", "NL", "NL", "DL", "NL", "NL", "TW"),
    List("NL", "DW", "NL", "NL", "NL", "TL", "NL", "NL", "NL", "TL", "NL", "NL", "NL", "DW", "NL"),
    List("NL", "NL", "DW", "NL", "NL", "NL", "DL", "NL", "DL", "NL", "NL", "NL", "DW", "NL", "NL"),
    List("DL", "NL", "NL", "DW", "NL", "NL", "NL", "DL", "NL", "NL", "NL", "DW", "NL", "NL", "DL"),
    List("NL", "NL", "NL", "NL", "DW", "NL", "NL", "NL", "NL", "NL", "DW", "NL", "NL", "NL", "NL"),
    List("NL", "TL", "NL", "NL", "NL", "TL", "NL", "NL", "NL", "TL", "NL", "NL", "NL", "TL", "NL"),
    List("NL", "NL", "DL", "NL", "NL", "NL", "DL", "NL", "DL", "NL", "NL", "NL", "DL", "NL", "NL"),
    List("TW", "NL", "NL", "DL", "NL", "NL", "NL", "NL", "NL", "NL", "NL", "DL", "NL", "NL", "TW"),
    List("NL", "NL", "DL", "NL", "NL", "NL", "DL", "NL", "DL", "NL", "NL", "NL", "DL", "NL", "NL"),
    List("NL", "TL", "NL", "NL", "NL", "TL", "NL", "NL", "NL", "TL", "NL", "NL", "NL", "TL", "NL"),
    List("NL", "NL", "NL", "NL", "DW", "NL", "NL", "NL", "NL", "NL", "DW", "NL", "NL", "NL", "NL"),
    List("DL", "NL", "NL", "DW", "NL", "NL", "NL", "DL", "NL", "NL", "NL", "DW", "NL", "NL", "DL"),
    List("NL", "NL", "DW", "NL", "NL", "NL", "DL", "NL", "DL", "NL", "NL", "NL", "DW", "NL", "NL"),
    List("NL", "DW", "NL", "NL", "NL", "TL", "NL", "NL", "NL", "TL", "NL", "NL", "NL", "DW", "NL"),
    List("TW", "NL", "NL", "DL", "NL", "NL", "NL", "TW", "NL", "NL", "NL", "DL", "NL", "NL", "TW")
  )

  def modify(string: String, scoreMap: Map[Char, Int], position: (Int, Int), direction: String): Map[Char, Int] = {
    val directionModifier = direction match {
      case "R" => 1
      case "D" => -1
    }


    def loop(string: String, scoreMap: Map[Char, Int], position: (Int, Int)): Map[Char, Int] = {

      if (string.isEmpty) return scoreMap
      scrabbleBoard(position._1)(position._2) match {
        case "DW" => loop(string.tail, scoreMap.map { case (c, i) => (c, i * 2) }, (position._1 + directionModifier *1, position._2))
        case "TW" => loop(string.tail, scoreMap.map { case (c, i) => (c, i * 3) }, (position._1 + directionModifier *1, position._2))
        case "NL" => loop(string.tail, scoreMap, (position._1 +  directionModifier * 1, position._2))
        case "DL" =>
          loop(string.tail,
               scoreMap.map { case (c, i) => if (c == string.head) (c, i * 2) else (c, i) },
               (position._1 + directionModifier*1, position._2))
        case "TL" =>
          loop(string.tail,
               scoreMap.map { case (c, i) => if (c == string.head) (c, i * 3) else (c, i) },
               (position._1 + directionModifier*1, position._2))
      }

    }

    val newPosition = direction match {
      case "R" => position
      case "D" => (position._2, position._1)
    }
    loop(string, scoreMap, newPosition)
  }

  def getWordValue(string: String, scoreMap: Map[Char, Int]): Int = {
    string.toList.map((x) => scoreMap(x)).sum
  }

}
