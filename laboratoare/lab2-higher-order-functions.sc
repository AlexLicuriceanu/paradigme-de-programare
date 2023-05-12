import scala.annotation.tailrec

// 2.1.1
def apply(n: Int, f: Int => Int): Int = {
  f(n)
}

// 2.1.2
def doubler(): Int => Int = {
  x => x * 2
}

apply(4, doubler())

// 2.2.1
def foldWith (op: (Int,Int) => Int)(start: Int, stop: Int): Int = {
  @tailrec
  def tail_fold(crt: Int, acc: Int): Int  = {
    if (crt > stop) acc
    else tail_fold(crt+1, op(crt, acc))
  }

  tail_fold(start, 0)
}

foldWith((x, y) => x + y)(1, 5)

// 2.2.2
def foldConditional(op: (Int,Int) => Int, p: Int => Boolean)(start: Int, stop: Int): Int = {
  @tailrec
  def tail_fold(crt: Int, acc: Int): Int = {
    if (crt > stop) acc
    else {
      if (p(crt)) tail_fold(crt + 1, op(crt, acc))
      else tail_fold(crt + 1, acc)
    }
  }

  tail_fold(start, 0)
}


foldConditional((x, y) => x + y, x => x % 2 == 1)(1, 3)

// 2.2.3
def foldMap(op: (Int,Int) => Int, f: Int => Int)(start: Int, stop: Int): Int = {
  @tailrec
  def tail_fold(crt: Int, acc: Int): Int = {
    if (crt > stop) acc
    else {
      tail_fold(crt + 1, op(f(crt), acc))
    }
  }

  tail_fold(start, 0)
}

foldMap((x, y) => x + y, doubler())(1, 6)

// 2.3.1
def multiply(x: Int)(y: Int): Int = x * y

multiply(5)(3)

// 2.3.2
def compare(x: Int)(y: Int)(z: Int): Int = {
  if (x > y && x > z)
    x
  else if (y > x && y > z)
    y
  else
    z
}

compare(2)(3)(6)

// 2.4.1
def shiftOY(line: Double => Double, delta_y: Double): Double => Double = {
  x => line(x) + delta_y - line(0)
}

// 2.4.2
def shiftOX(line: Double => Double, delta_x: Double): Double => Double = {
  x => line(x - delta_x)
}

// 2.4.3
def intersect(line1: Double => Double, line2: Double => Double)(start: Int, stop: Int): Boolean = {
  @tailrec
  def aux_intersect(start: Int, stop: Int): Boolean = {
    if (start > stop) false
    else {
      if (line1(start) == line2(start)) true
      else aux_intersect(start + 1, stop)
    }
  }

  aux_intersect(start, stop)
}

intersect(x => 2*x + 1, x => (-2)*x + 1)(-2, 3)