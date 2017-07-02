package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class TautologyTest extends FlatSpec {

  behavior of "Tautology"

  it should "list variables parsed from an expression" in {
    Tautology.listVariables("a|(b&c)") should be(List('a', 'b', 'c'))
  }

  it should "given a list, it should be able to tell if the variable count is balanced" in {
    Tautology.isVariableCountBalanced(List('a', 'b', 'a', 'c')) should be(false)
    Tautology.isVariableCountBalanced(List('a','b', 'a', 'b')) should be(true)
  }

}
