package in.extrasalt.scalapblms

object Prime {
  def main(args: Array[String]) = {
    assert(listPrimesinRange(7 to 31).equals(List(7, 11, 13, 17, 19, 23, 29, 31)))
  }

  def isPrime(n: Int) = {
    val s = (2 to n / 2).toList.map((x) => n / x).sum
    s == 0
  }

  def listPrimesinRange(range: Range): List[Int] = {
    range.toList.filter((x) => isPrime(x))
  }
}

