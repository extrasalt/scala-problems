package in.extrasalt.scalapblms

class MarsRover {
  class Rover(x: Int, y: Int, dir: String){
    def moveForward(): Rover = dir match {
      case "W" => new Rover(x-1, y, dir)
      case "E" => new Rover(x+1, y, dir)
      case "S" => new Rover(x, y-1, dir)
      case "N" => new Rover(x, y+1, dir)
    }

    def rotateLeft(): Rover = dir match {
      case "W" => new Rover(x, y,"S")
      case "E" => new Rover(x, y,"N")
      case "S" => new Rover(x, y,"E")
      case "N" => new Rover(x, y,"W")
    }

    def rotateRight(): Rover = dir match {
      case "W" => new Rover(x, y,"N")
      case "E" => new Rover(x, y,"S")
      case "S" => new Rover(x, y,"W")
      case "N" => new Rover(x, y,"E")
    }

    def position(): String = {
      s"$x $y $dir"
    }

    def handleInputString(s : String) = {
      val list = s.toIndexedSeq
      list.foldLeft(this)((r:Rover, x: Char) => x match{
        case 'L' => r.rotateLeft()
        case 'R' => r.rotateRight()
        case 'M' => r.moveForward()
      })
    }
  }
}
