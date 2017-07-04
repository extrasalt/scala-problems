package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class TautologyTest extends FlatSpec {

  behavior of "Tautology"

  it should "be able to tell if the variable count is balanced in a given list" in {
    Tautology.isVariableCountBalanced("a|b&c") should be(false)
    Tautology.isVariableCountBalanced("(a&b)|(a|b)") should be(true)
  }

  it should "convert infix to postfix when expression has no braces" in {
    Tautology.convertToPostfix("a&b") should be("ab&")
    Tautology.convertToPostfix("a&b|c") should be("abc|&")
  }

  it should "convert infix to postfix when expression has braces" in {
    Tautology.convertToPostfix("a&(b|c)") should be("abc|&")
  }

}
