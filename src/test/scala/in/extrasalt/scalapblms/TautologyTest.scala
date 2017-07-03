package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class TautologyTest extends FlatSpec {

  behavior of "Tautology"

  it should "list variables parsed from an expression" in {
    Tautology.listVariables("a|(b&c)") should be(List('a', 'b', 'c'))
  }

  it should "be able to tell if the variable count is balanced in a given list" in {
    Tautology.isVariableCountBalanced(List('a', 'b', 'a', 'c')) should be(false)
    Tautology.isVariableCountBalanced(List('a','b', 'a', 'b')) should be(true)
  }

  it should "evaluate and return 1 for tautological cases" in {
    Tautology.eval("a|!a") should be("1")
    Tautology.eval("!a|a") should be("1")
  }

  it should "split at opening braces and return two Lists" in {
    Tautology.splitAtOpeningBraces("a | ( b & c)") should be(List("a|","b&c)"))
  }

  it should "split at closing braces and return two lists" in {
    Tautology.splitAtClosingBraces("b&c)d)") should be(List("b&c","d)"))
    Tautology.splitAtClosingBraces("d)") should be(List("d"))
  }

  "isTautology" should "return false if variable count is unbalanced" in {
    Tautology.isTautology("a|(b|c)") should be(false)
  }

  "eval" should "correctly simplify nested expressions" in {
    Tautology.eval("(a|(b&c))") should be("0")
  }

}
