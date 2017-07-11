package in.extrasalt.scalapblms

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class RoverTest extends FlatSpec {

  "Rover" should "print position" in {
    Rover(0, 0, "E").position() should be("0 0 E")
  }

  it should "move forward" in {
    Rover(0,0, "E").moveForward().position() should be("1 0 E")
    Rover(2,2, "W").moveForward().position() should be("1 2 W")
    Rover(2,2, "N").moveForward().position() should be("2 3 N")
    Rover(2,2, "S").moveForward().position() should be("2 1 S")
  }

  it should "rotate left" in {
    Rover(0, 0, "E").rotateLeft().position() should be("0 0 N")
  }

  it should "rotate right" in {
    Rover(0, 0, "E").rotateRight().position() should be("0 0 S")
  }

  it should "handle input in the form of a string" in {
    Rover(0, 0, "E").handleInputString("MMLM").position() should be("2 1 N")
  }

  it should "come back to the same direction after 4 turns" in {
    Rover(0, 0, "E").handleInputString("LLLL").position() should be("0 0 E")
    Rover(0, 0, "E").handleInputString("RRRR").position() should be("0 0 E")
  }
}
