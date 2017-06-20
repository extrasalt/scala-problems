package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class PrimeTest extends FlatSpec {

  "Prime" should "verify prime numbers in range" in {
    Prime.listPrimesinRange(7 to 31) should be(List(7, 11, 13, 17, 19, 23, 29, 31))
  }

}
