package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class BarterTest extends FlatSpec {

  "Barter" should "create a map of prices from exclamation inputs" in {
    Barter.createStockExchange(List("! 4 potatoes = 5 tomatoes")) should be(Map("potatoes" -> 1000.0, "tomatoes" -> 800.0))
  }

  "QuestionStatement" should "return the value of one product in terms of another product using a given map" in {
    new QuestionStatement("? potatoes = tomatoes").findRatio(Map("potatoes" -> 1000.0, "tomatoes" -> 800.0)) should be(1)
  }

  "Barter" should "parse a list of inputs and return a list of inputs" in {
    Barter.parseInput(List("! 4 potatoes = 5 tomatoes", "! 2 potatoes = 10 bananas", "? bananas = potatoes", "? bananas = tomatoes")) should be(List(5,4))
  }

}
