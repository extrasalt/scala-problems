package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class BarterTest extends FlatSpec {

  "Barter" should "create a map of prices from exclamation inputs" in {
    Barter.createStockExchange(List("! 4 potatoes = 5 tomatoes")) should be(Map("potatoes" -> 1000.0, "tomatoes" -> 800.0))
  }

  "Barter" should "return the count of one product in terms of another product" in {
    Barter.findRatio("? potatoes = tomatoes",Map("potatoes" -> 1000.0, "tomatoes" -> 800.0)) should be(1)
  }

}
