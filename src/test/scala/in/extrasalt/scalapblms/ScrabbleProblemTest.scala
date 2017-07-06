package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class ScrabbleProblemTest extends FlatSpec {

    "Scrabble" should "return the value of the given string" in {
      val scoreMap: Map[Char, Int] = Map(
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
      
      ScrabbleProblem.getWordValue("indix", scoreMap) should be(13)
    }

    "Scrabble board" should "have a proper count of all modifiers" in {
      ScrabbleProblem.scrabbleBoard.values.count((x) => x == "triple word") should be(8)
      ScrabbleProblem.scrabbleBoard.values.count((x) => x == "triple letter") should be(12)
      ScrabbleProblem.scrabbleBoard.values.count((x) => x == "double word") should be(16)
      ScrabbleProblem.scrabbleBoard.values.count((x) => x == "double letter") should be(24)


    }


}
