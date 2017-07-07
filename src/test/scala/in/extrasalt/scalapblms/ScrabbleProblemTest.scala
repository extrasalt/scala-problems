package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class ScrabbleProblemTest extends FlatSpec {

  "Scrabble board" should "have a proper count of all modifiers" in {
    ScrabbleProblem.scrabbleBoard.flatten.count((x) => x == "TW") should be(8)
    ScrabbleProblem.scrabbleBoard.flatten.count((x) => x == "DL") should be(24)
    ScrabbleProblem.scrabbleBoard.flatten.count((x) => x == "TL") should be(12)
    ScrabbleProblem.scrabbleBoard.flatten.count((x) => x == "DW") should be(16)

  }

  it should "return a word value for string starting at 0, 0 R" in {
    ScrabbleProblem.getWordValue("indix", ScrabbleProblem.defaultScoreMap) should be(13)

  }

  it should "modify score map" in {
    val modifiedMap  = ScrabbleProblem.modify("indix", ScrabbleProblem.defaultScoreMap)
    val modifiedMap2 = ScrabbleProblem.modify("unconsciousness", ScrabbleProblem.defaultScoreMap)
    ScrabbleProblem.getWordValue("indix", modifiedMap) should be(45)
    ScrabbleProblem.getWordValue("unconsciousness", modifiedMap2) should be(648)
  }

}
