package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class TautologyTest extends FlatSpec {

  behavior of "Tautology"

  it should "convert infix to postfix when expression has no braces" in {
    new Expression("a&b").postfix should be("ab&")
    new Expression("a&b|c").postfix should be("abc|&")
  }

  it should "convert infix to postfix when expression has braces" in {
    new Expression("a&(b|c)").postfix should be("abc|&")
    new Expression("(a&b)|c").postfix should be("ab&c|")
    new Expression("a|!a").postfix should be("aa!|")
  }

  it should "generate a truth table for 2 variables" in {
    val truthTable = TruthTable.generateTruthTable(Set("a", "b", "c"))
    truthTable should contain(Map("a" -> true, "b"  -> true, "c"  -> true))
    truthTable should contain(Map("a" -> true, "b"  -> false, "c" -> true))
    truthTable should contain(Map("a" -> false, "b" -> true, "c"  -> true))
    truthTable should contain(Map("a" -> false, "b" -> false, "c" -> true))
    truthTable should contain(Map("a" -> true, "b"  -> true, "c"  -> false))
    truthTable should contain(Map("a" -> true, "b"  -> false, "c" -> false))
    truthTable should contain(Map("a" -> false, "b" -> true, "c"  -> false))
    truthTable should contain(Map("a" -> false, "b" -> false, "c" -> false))
  }

  it should "generate a truth table for one variable" in {
    val truthTable = TruthTable.generateTruthTable(Set("a"))
    truthTable should contain(Map("a" -> true))
    truthTable should contain(Map("a" -> false))
  }

  it should "evaluate postfix expression" in {
    new Postfix("abc|&").evaluate should be(false)
    new Postfix("aa!|").evaluate should be(true)
  }

}
