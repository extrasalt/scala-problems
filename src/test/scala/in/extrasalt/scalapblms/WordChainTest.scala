package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class WordChainTest extends FlatSpec {
  "WordChain" should "return false if the size of inputs don't match" in {
    new WordChain("yellow", "hello").chainExists() should be(false)
  }
}