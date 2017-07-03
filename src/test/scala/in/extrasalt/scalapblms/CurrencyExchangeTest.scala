package in.extrasalt.scalapblms

import org.scalatest.FlatSpec

class CurrencyExchangeTest extends FlatSpec {

  "CurrencyExchange" should "return roman characters for given question" in {
    new CurrencyExchange(Map("glob" -> "I", "prok" -> "V"))
  }

}
