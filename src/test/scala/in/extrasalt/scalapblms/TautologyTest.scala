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
    Tautology.eval("a | !a") should be("1")
    Tautology.eval("!a | a") should be("1")
  }

  it should "split at braces and return two Lists" in {
    Tautology.splitAtBraces("a | ( b & c)") should be(List("a | "," b & c)"))
  }

}
