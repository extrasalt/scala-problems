package in.extrasalt.scalapblms

case class Plateau(x: Int, y:Int)

case class Rover(x: Int, y: Int, dir: String, plateau: Plateau) {
  def position(): String = {
    s"$x $y $dir"
  }

  def handleInputString(s: String) = {
    val list = s.toIndexedSeq
    list.foldLeft(this)((r: Rover, x: Char) => x match {
      case 'L' => r.rotateLeft()
      case 'R' => r.rotateRight()
      case 'M' => r.moveForward()
    })
  }

  def moveForward(): Rover = dir match {
    case "W" => this.copy(x= x - 1)
    case "E" => this.copy(x = x + 1)
    case "S" => this.copy(y = y - 1)
    case "N" => this.copy(y = y + 1)
  }

  def rotateLeft(): Rover = dir match {
    case "W" => this.copy(dir="S")
    case "E" => this.copy(dir="N")
    case "S" => this.copy(dir="E")
    case "N" => this.copy(dir="W")
  }

  def rotateRight(): Rover = dir match {
    case "W" => this.copy(dir="N")
    case "E" => this.copy(dir="S")
    case "S" => this.copy(dir="W")
    case "N" => this.copy(dir="E")
  }

}
