package in.extrasalt.scalapblms

class Score(scoreMap: Map[Char, Int]) {
  val dictionary = scoreMap
  def DW: Score = {
    new Score(scoreMap.map { case (c, i) => (c, i * 2) })
  }

  def TW: Score = {
    new Score(scoreMap.map { case (c, i) => (c, i * 3) })
  }

  def DL(letter: Char): Score = {
    new Score(scoreMap.map { case (c, i) => if (c == letter) (c, i * 2) else (c, i) })
  }

  def TL(letter: Char): Score = {
    new Score(scoreMap.map { case (c, i) => if (c == letter) (c, i * 3) else (c, i) })
  }
}

object Score{
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
}

object ScrabbleProblem {


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

  def modify(string: String, scoreMap: Score, position: (Int, Int), direction: String): Score = {
    val directionModifier = direction match {
      case "R" => 1
      case "D" => -1
    }

    def loop(string: String, scores: Score, position: (Int, Int)): Score= {

      val (x, y) = position

      if (string.isEmpty) return scores
      scrabbleBoard(x)(y) match {
        case "DW" => loop(string.tail, scores.DW, (x + directionModifier * 1, y))
        case "TW" => loop(string.tail, scores.TW, (x + directionModifier * 1, y))
        case "NL" => loop(string.tail, scores, (x + directionModifier * 1, y))
        case "DL" =>
          loop(string.tail, scores.DL(string.head), (x + directionModifier * 1, y))
        case "TL" =>
          loop(string.tail, scores.TL(string.head), (x + directionModifier * 1, y))
      }

    }

    val newPosition = direction match {
      case "R" => position
      case "D" => position.swap
    }
    loop(string, scoreMap, newPosition)
  }

  def getWordValue(string: String, scoreMap: Score): Int = {
    string.toList.map((x) => scoreMap.dictionary(x)).sum
  }

}
