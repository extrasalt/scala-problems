package in.extrasalt.scalapblms

class WordChain(start: String, end: String) {

  def chainExists(): Boolean = {
    if (start.size != end.size) {
      return false
    }
    return true
  }

}
