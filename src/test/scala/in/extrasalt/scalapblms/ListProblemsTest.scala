package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class ListProblemsTest extends FlatSpec {
  "List" should "return last element" in {
    ListProblems.last(List(1, 1, 2, 3, 5, 8)) should be(8)
  }

  "List" should "compress" in {
    ListProblems.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List('a, 'b, 'c, 'd, 'e))
  }

  "List" should "reverse" in {
    ListProblems.reverse(List(1, 1, 2, 3, 5, 8)) should be(List(8, 5, 3, 2, 1, 1))
  }

  "List" should "penultimate" in {
    ListProblems.penultimate(List(1, 1, 2, 3, 5, 8)) should be(5)
  }

  "List" should "sum3sAnd5s" in {
    ListProblems.sum3sAnd5s(1 to 9) should be(23)
  }

  "List" should "slice" in {
    ListProblems.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))should be(List('d, 'e, 'f, 'g))
  }

  "List" should "drop" in {
    ListProblems.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

}
