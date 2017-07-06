package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._


class CurrencyExchangeTest extends FlatSpec {

  "CurrencyExchange" should "convert roman numerals into actual numbers" in {
    CurrencyExchange.convert("MCMIII") should be(1903)
  }

}
