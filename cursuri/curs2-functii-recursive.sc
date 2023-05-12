import scala.annotation.tailrec

val x = 0
"Matei"

val b = if (x > 0) 1 else 0

var y = 0
def f(x: Int): Int = {
  var y = x
  y = y + 1
  y
}
// definitia unei functii
// care se numeste f
// si are un parametru de tip int, care se numeste x
// f intoarce un Int
// expresia la care se evalueaza f este x + 1

f(1)

def myAddition(x: Int, y: Int): Int = x + y
myAddition(2,3)

def inRange(start: Int, stop: Int, x: Int):Boolean =
  x >= start && x <= stop


def fib(n: Int): Int =
  if (n == 0) 0
  else if (n == 1) 1
  else fib(n-1) + fib(n-2)

def fib_better(n: Int): Int = {
  @tailrec
  def fib_aux(n: Int,
              last: Int,
              before_last: Int): Int =
    if (n == 1) last
    else fib_aux(n-1,before_last + last, last)

  fib_aux(n,1,0)
}

fib_better(500)

// f0 = 0, f1 = 1, f2 = 1, f3 =

def fact(n: Int):Int =
  if (n == 0) 1
  else n * fact(n-1)


def fact(n: Int): Int = {
  def fact_aux(n: Int, acc: Int): Int =
    if (n == 0) acc
    else fact_aux(n-1, n * acc)
  fact_aux(n,1)
}

// vrem sa calculam suma tuturor numerelor prime
// dintr-un interval

def sumPrimes(start: Int, stop: Int): Int = {
  def isPrime(x: Int): Boolean = true

  def aux_sum(i: Int, acc: Int): Int = {
    if (i > stop) acc
    else if (isPrime(i)) aux_sum(i+1, i + acc)
    else aux_sum(i+1,acc)
  }

  aux_sum(start,0)
}
sumPrimes(1,10)
