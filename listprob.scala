object ListManip{
  def last(list : List[Int]) : Int = {
    list(list.length - 1)
  }

  def penultimate(list : List[Int]) : Int =  {
    list(list.length - 2)
  }
  
  def main(args: Array[String]) = {
    assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
    assert( penultimate(List(1, 1, 2, 3, 5, 8)) == 5 )
  }
}
