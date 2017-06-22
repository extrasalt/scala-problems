package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class RoverTest extends FlatSpec {

  "Rover" should "print position" in {
    new Rover(0, 0, "E").position() should be("0 0 E")
  }

  "Rover" should "move forward" in {
    new Rover(0,0, "E").moveForward().position() should be("1 0 E")
    new Rover(2,2, "W").moveForward().position() should be("1 2 W")
    new Rover(2,2, "N").moveForward().position() should be("2 3 N")
    new Rover(2,2, "S").moveForward().position() should be("2 1 S")
  }

  "Rover" should "rotate left" in {
    new Rover(0, 0, "E").rotateLeft().position() should be("0 0 N")
  }

  "Rover" should "rotate right" in {
    new Rover(0, 0, "E").rotateRight().position() should be("0 0 S")
  }

}
