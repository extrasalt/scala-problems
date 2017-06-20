object ListManip{
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
  
  def main(args: Array[String]) = {
    assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
    assert( penultimate(List(1, 1, 2, 3, 5, 8)) == 5 )
    assert( compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'd, 'e) )
  }
}
