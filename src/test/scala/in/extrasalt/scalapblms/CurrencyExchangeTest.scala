package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class CurrencyExchangeTest extends FlatSpec {

  "CurrencyExchange" should "convert roman numerals into actual numbers" in {
    CurrencyExchange.convert("MCMIII") should be(1903)
    CurrencyExchange.convert("XCI") should be(91)
    CurrencyExchange.convert("CDXLI") should be(441)
    CurrencyExchange.convert("CMIX") should be(909)
    CurrencyExchange.convert("DCCLXII") should be(762)
    CurrencyExchange.convert("XV") should be(15)

  }

  it should "parse a bad statement to return default response. " in {
    CurrencyExchange.parse("how much wood could a woodchuck chuck if a woodchuck could chuck wood ?",
                           Map("grob" -> 'I'),
                           Map()) should be("I have no idea what you are talking about")

  }

  it should "parse a given statement and return the value" in {
    val dictionary = Map(
      "glob" -> 'I',
      "prok" -> 'V',
      "pish" -> 'X',
      "tegj" -> 'L'
    )

    val valueMap = Map(
      "Silver" -> 17,
      "Gold"   -> 14675,
      "Iron"   -> 195
    )

    CurrencyExchange.parse("how much is pish tegj glob glob ?", dictionary, valueMap) should be(
      "pish tegj glob glob is 42")

    CurrencyExchange.parse("how many Credits is glob prok Silver ?", dictionary, valueMap) should be(
      "glob prok Silver is 68 Credits")
  }

}
