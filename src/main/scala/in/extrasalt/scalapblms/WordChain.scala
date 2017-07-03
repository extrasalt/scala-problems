package in.extrasalt.scalapblms

import scala.io.Source

class WordChain(dictionary: List[String]) {


  val adjacentList: Map[String, Set[String]] = {

    dictionary.map((x) => x -> getAdjacentElements(x)).toMap
  }

  def getAdjacentElements(string: String): Set[String] = {
    dictionary.filter(WordChain.countHopAway(string, _) == 1).toSet
  }

  def chainExists(start: String, end: String) : Boolean = {
//    val filterdDictionary: List[String] = dictionary.filter(_.length == start.length)
    def _chainExists(start: String, visited: Set[String]): Boolean = {
      val newVisited: Set[String] = visited ++ Set(start)
      if (start.length != end.length) false
      else start.equals(end) || adjacentList(start).diff(visited).exists((x) => _chainExists(x, newVisited))

    }

    _chainExists(start, visited=Set())
  }

}

object WordChain {
  def apply(dictionary: List[String]): WordChain = {

    new WordChain(dictionary)
  }

  def readDictionary(): List[String] = {
    Source.fromFile("/words.dat").getLines.toList
  }

  def countHopAway(a: String, b: String, i: Int = 0, count: Int = 0): Int = {

    if (i == a.length) { return count }
    val augmentedString = augmentString(b, a, i)
    if (augmentedString == b) countHopAway(a, augmentedString, i + 1, count)
    else countHopAway(a, augmentedString, i + 1, count + 1)
  }

  def augmentString(s: String, from: String, at: Int): String = {
    val aList = from.toList
    val bList = s.toList

    val augmentedB = bList.zipWithIndex.map {
      case (c, i) => if (i == at) aList(at) else c
    }

    augmentedB.mkString

  }

}
