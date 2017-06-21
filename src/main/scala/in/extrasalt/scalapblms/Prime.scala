package in.extrasalt.scalapblms

object Prime {
  def main(args: Array[String]) = {
  }

  def isPrime(n: Int) = {
    val s = (2 to n / 2).toList.map((x) => n % x).sum
    s == 0
  }

  def listPrimesinRange(range: Range): List[Int] = {
    range.toList.filter((x) => isPrime(x))
  }
}

