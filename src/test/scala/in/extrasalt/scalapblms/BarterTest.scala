package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class BarterTest extends FlatSpec {

  "Barter" should "create a map of prices from exclamation inputs" in {
    Barter.createStockExchange(List("! 4 potatoes = 5 tomatoes")) should be(Map("potatoes" -> 1000.0, "tomatoes" -> 800.0))
  }

}
