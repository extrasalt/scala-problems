package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class WordChainTest extends FlatSpec {
  "WordChain" should "return false if the size of inputs don't match" in {
    new WordChain("yellow", "hello", List("card", "tomato")).chainExists() should be(false)
  }

  "WordChain" should "return correct number of hops away" in {
    WordChain.countHopAway("yellow", "hellow") should be(1)
    WordChain.countHopAway("too", "tre") should be(2)
  }

  "WordChain" should "be able to augment a string with another string" in {
    WordChain.augmentString("Hello", "Tryan", 2) should be("Heylo")
  }

  it should "be able to generate an adjacency list for a given word" in {
    val wc = new WordChain("yello", "hello", List("hello", "yallo", "herro", "terro", "herrp"))
    wc.getAdjacentElements("yello").toSet should be(List("yallo", "hello").toSet)
    wc.getAdjacentElements("herro").toSet should be(List("terro", "herrp").toSet)
  }
}