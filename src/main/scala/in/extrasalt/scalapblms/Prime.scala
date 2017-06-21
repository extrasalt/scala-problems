package in.extrasalt.scalapblms

object Prime {
  def main(args: Array[String]) = {
  }

  def isPrime(n: Int) = {
    val s = (2 to n / 2).toList.map((x) => n % x).contains(0)
    !s
  }

  def listPrimesinRange(range: Range): List[Int] = {
    range.toList.filter((x) => isPrime(x))
  }
}

