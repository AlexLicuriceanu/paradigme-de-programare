import scala.annotation.tailrec
import scala.math._

def fact (n: Int): Int = {
  @tailrec
  def aux_fact(n: Int, acc: Int): Int =
    if (n == 0) acc
    else aux_fact(n-1, n*acc)

  aux_fact(n, 1)
}

fact(3)   // = 6

@tailrec
def gcd(a: Int, b: Int): Int = {
  if (b == 0) a
  else gcd(b, a % b)
}

gcd(12, 18)   // = 6

def sumSquares(n: Int): Int = {
  @tailrec
  def aux_sumSquares(n: Int, acc: Int): Int = {
    if (n == 0) acc
    else aux_sumSquares(n-1, n*n + acc)
  }

  aux_sumSquares(n, 0)
}

sumSquares(3)   // = 14

def sumNats(start: Int, stop: Int): Int = {
  if (start > stop) 0
  else start + sumNats(start+1, stop)
}

sumNats(1, 8)   // = 36

def tailSumNats(start: Int, stop: Int): Int = {
  @tailrec
  def aux_tailSumNats(start: Int, stop: Int, acc: Int): Int = {
    if (start > stop) acc
    else aux_tailSumNats(start+1, stop, start + acc)
  }

  aux_tailSumNats(start, stop, 0)
}

tailSumNats(1, 8)   // = 36

def sumPrimes(start: Int, stop: Int): Int = {
  def isPrime(n: Int): Boolean = {
    @tailrec
    def aux_isPrime(n: Int, i: Int, acc: Int): Boolean = {
      //i == n || n % i != 0 && aux_isPrime(n, i+1)
      if (n == 1 || n < 0) false
      else {
        if (i * i > n) true
        else {
          if (n % i == 0) false
          else aux_isPrime(n, i+1, acc + n % i)
        }
      }
    }

    if (aux_isPrime(n, 2, 0)) true
    else false
  }

  @tailrec
  def aux_sumPrimes(start: Int, stop: Int, acc: Int): Int = {
    if (start > stop) acc
    else {
      if (isPrime(start)) aux_sumPrimes(start+1, stop, start+acc)
      else aux_sumPrimes(start+1, stop, acc)
    }
  }

  aux_sumPrimes(start, stop, 0)
}

sumPrimes(-1, 10)    // = 17

def improve(xn: Double, a: Double): Double = (xn + a/xn) / 2

def nth_guess(n: Int, a: Double): Double = {
  @tailrec
  def aux_nth_guess(n: Int, a: Double, xn: Double): Double = {
    if (n == 0) xn
    else aux_nth_guess(n-1, a, improve(xn, a))
  }

  aux_nth_guess(n, a, 1)
}

def acceptable(xn: Double, a: Double): Boolean =
  if (abs(xn*xn - a) <= 0.001) true
  else false

def mySqrt(a: Double): Double = {
  def improve(xn: Double): Double = (xn + a/xn) / 2

  def acceptable(xn: Double): Boolean =
    if (abs(xn * xn - a) <= 0.001) true
    else false

  @tailrec
  def tailSqrt(estimate: Double): Double =
    if (acceptable(estimate)) estimate
    else tailSqrt(improve(estimate))

  tailSqrt(1)
}

mySqrt(3)   // 1.732...
