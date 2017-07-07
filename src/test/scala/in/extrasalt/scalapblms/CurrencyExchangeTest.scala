package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class CurrencyExchangeTest extends FlatSpec {

  "CurrencyExchange" should "convert roman numerals into actual numbers" in {
    CurrencyExchange.convert("MCMIII") should be(1903)
  }

  it should "parse a given statement" in {
    CurrencyExchange.parse("how much wood could a woodchuck chuck if a woodchuck could chuck wood ?",
                           Map("grob" -> 'I')) should be(
      "I have no idea what you are talking about")

  }

}
