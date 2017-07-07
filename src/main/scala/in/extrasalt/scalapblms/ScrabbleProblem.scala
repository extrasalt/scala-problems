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


  def modify(string: String, scoreMap: Map[Char, Int]): Map[Char, Int] = {

    def loop(string: String, scoreMap: Map[Char, Int]): Map[Char, Int] ={
      loop(string.tail, scoreMap.map{case (c,i)=>(c,i*2)})
    }

    scoreMap
  }

  def getWordValue(string: String, scoreMap: Map[Char, Int]): Int = {
    string.toList.map((x) => scoreMap(x)).sum
  }

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

}
