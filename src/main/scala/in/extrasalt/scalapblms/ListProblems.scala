package in.extrasalt.scalapblms

object ListProblems {
  def last(list : List[Int], n : Int = 0) : Int = {
    if(n==list.length-1) list(n)
    else {
      last(list, n+1)
    }
  }

  def penultimate(list : List[Int], n: Int=0) : Int =  {
    if(n==list.length-2) list(n)
    else {
      penultimate(list, n+1)
    }
  }

  def compress(list: List[Symbol]) : List[Symbol] = {
    if (list.size == 0) list
    else List(list(0)) ::: compress(list.filter((x) => x != list(0)))
  }

  def sum3sAnd5s(range: Range) : Int = {
    range.toList.filter((x) => x % 3 == 0 || x %5 == 0).reduce((x,y) => x+y)
  }

  def drop(n : Int, list : List[Symbol]) : List[Symbol] = {
    if (list.size < n) {
      list
    }
    else {
      val (left, right) = list.splitAt(n-1)
      left ::: drop(n, right.drop(1))
    }
  }

  def slice(l : Int, r : Int, list : List[Symbol])={
    val (left, right) = list.splitAt(r)
    left.drop(l)
  }

  def reverse(list : List[Int]) : List[Int] = {
    if(list.size == 1) list
    else {
      val (left, right) = list.splitAt(list.length-1)
      right ::: reverse(left)
    }
  }

  def isLex(n : Int, s: Set[Int]): Boolean = {
    //Number as list of digits
    val list = n.toString.map(_.asDigit).toList
    //Checks if the number contains unique digits
    if(list.size != list.toSet.size) return false
    else {
      list.toSet.subsetOf(s)
    }
  }

  def findAllPermutation(s: Set[Int]) : List[Int] = {
    val limit = (math.pow(10,s.size) - 1).toInt
    val start = (math.pow(10,s.size-1)).toInt
    (start to limit).toList.filter((x)=> isLex(x, s))
  }

  def main(args: Array[String]) = {

    println(sum3sAnd5s(1 to 999))
  }
}
