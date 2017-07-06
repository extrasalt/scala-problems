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

  def getWordValue(string: String, scoreMap: Map[Char, Int]) = {
    string.toList.map((x) => scoreMap(x)).sum
  }

  val scrabbleBoard: Map[(Int, Int), String] = {
    val matrix = (0 to 14).toList.flatMap((x) => (0 to 14).toList.map((y) => (x, y)))
    val dlSet = {
      matrix.filter((t) =>
        (t._1, t._2) match {
          case (x, y) if (x == 6 || x == 8) && (y == 6 || y == 8) => true
          case (3, 7) | (7, 3) | (11, 7) | (7, 11)                => true
          case (x, y) if (x % 8 == 3) && (y % 7 == 0) => true
          case (x, y) if (y % 8 == 3) && (x % 7 == 0) => true
          case (x, y) if Set(2, 6, 8, 12).contains(x) && Set(2, 6, 8, 12).contains(y) && x != y => true
          case _                                                                                => false
      })
    }

    val modifiers = matrix.map((t) =>
      (t._1, t._2) match {
        case (7, 7) => "*"
        case (x, y) if x % 7 == 0 && y % 7 == 0 => "triple word"
        case (x, y) if x % 4 == 1 && y % 4 == 1 &&  (x != y) => "triple letter"
        case (x, y) if (x == 9 || x ==5) && (y == 9 || y == 5) => "triple letter"
        case (x, y) if (x % 9 < 5) && (y % 9 <5) && (x ==y || x == 14-y)  => "double word"

        case (x, y) if dlSet.contains((x, y)) => "double letter"

        case _                                => "normal"
    })

    matrix.zip(modifiers).toMap

  }

}
