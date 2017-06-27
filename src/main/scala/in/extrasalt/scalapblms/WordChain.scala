package in.extrasalt.scalapblms

import scala.io.Source

class WordChain(start: String, end: String, dictionary: List[String]) {

  val filterdDictionary: List[String] = dictionary.filter(_.length == start.length)

  def adjacentList: Map[String, List[String]] = {

    this.filterdDictionary.map((x) => x -> getAdjacentElements(x)).toMap
  }

  def getAdjacentElements(string: String): List[String] = {
    filterdDictionary.filter(WordChain.countHopAway(string, _) == 1)
  }

  def chainExists(start: String = start, visited: Set[String] = Set()): Boolean = {
    if(visited.contains(start)) return false
    val newVisited : Set[String] = visited ++ Set(start)
    if (start.length != end.length) false
    else if (start.equals(end)) true
    else if (adjacentList(start).toSet.subsetOf(visited)) false
    else adjacentList(start).exists((x)=>chainExists(x,newVisited))

  }

}

object WordChain {

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
