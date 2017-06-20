package in.extrasalt.scalapblms

/**
  * Created by mohan on 20/6/17.
  */
object ListProblems {
  def last(list : List[Int]) : Int = {
    list(list.length - 1)
  }

  def penultimate(list : List[Int]) : Int =  {
    list(list.length - 2)
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

  def main(args: Array[String]) = {
    assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
    assert( penultimate(List(1, 1, 2, 3, 5, 8)) == 5 )
    assert( compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'd, 'e) )
    assert(sum3sAnd5s(1 to 9) == 23)
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)).equals(List('d, 'e, 'f, 'g)))
    assert(reverse(List(1, 1, 2, 3, 5, 8)).equals(List(8, 5, 3, 2, 1, 1)))
    println(sum3sAnd5s(1 to 999))
  }
}
