trait Expr {
  def eval(): Int
}

case class Atom(x: Int) extends Expr {
  override def eval(): Int = x
}

case class Add(e1: Expr, e2: Expr) extends Expr {
  override def eval(): Int = e1.eval() + e2.eval()
}

case class Mult(e1: Expr, e2: Expr) extends Expr {
  override def eval(): Int = e1.eval() * e2.eval()
}

Add(Atom(1), Mult(Atom(2), Atom(3))).eval()

/*
  - calculeaza valoarea unei expresii
  - group:
    e1 * e2 + e1 * e3 = e1 * (e2 + e3)
 */

def eval(e: Expr): Int = ???

def group(e: Expr): Expr =
  e match {
    case Add(Mult(e1, e2), Mult(e3, e4)) =>
      if (e1 == e3) Mult(e1, Add(e2, e4))
      else e1 ///
  }


/*
  Liste
 */
val l1 = List(1, 2, 3, 4)
val l2 = 1 :: 2 :: 3 :: 4 :: Nil

l1.head
l2.tail

def sum(l: List[Int]): Int =
  l match {
    case Nil => 0
    case x::xs => x + sum(xs)
  }

def prod(l: List[Int]): Int =
  l match {
    case Nil => 1
    case x::xs => x * prod(xs)
  }

// valoarea initiala
// operatia

def fold1(b: Int)(op: (Int, Int) => Int)(l: List[Int]): Int =
  l match {
    case Nil => b
    case x::xs => op(x, fold1(b)(op)(xs))
  }

// (x+y) => x+y
val sum1: List[Int] =>Int = fold1(0)(_ + _)

// 1-(2-(3-100))
fold1(100)(_ - _)(List(1,2,3))

// ((b @ x1) @ x2) @ x3
def foldLeft(b: Int)(op: (Int, Int) => Int)(l: List[Int]): Int = {
  def loop(acc: Int, l: List[Int]): Int = {
    l match {
      case Nil => acc
      case x::xs => loop(op(acc, x), xs)
    }
  }
  loop(b, l)
}

List(1,2,3).foldRight(0)(_ + _)
List(1,2,3).foldLeft(0)(_ + _)
/*
def reverse(l: List[Int]): List[Int] =
  l.foldLeft(Nil: List[Int])((acc, x) => x :: acc)
 */

def reverse(l: List[Int]): List[Int] = {
  def loop(l: List[Int], acc: List[Int]): List[Int] =
    l match {
      case Nil => acc
      case x::xs => loop(xs, x::acc)
    }

  loop(l, Nil)
}

reverse(List(1,2,3,4))
/*
  reverse:
    ((b @ x1) @ x2) @ x3
    x @ y este introdu pe y in lista x
 */

/*
  Avem un sir care contine numere si whitespace
  12 4 560 3

  Vrem sa despartim sirul dupa caracterul whitespace
  "12", "4", "560", "3"
 */

type Sir = List[Char]

def split(s: Sir): List[Sir] = {
  def op(c: Char, acc: List[Sir]): List[Sir] = {
    acc match {
      case Nil => if (c == ' ') Nil else List(List(c))
      case x::xs => if (c == ' ') Nil::acc else (c::x)::xs
    }
  }
  s.foldRight(Nil: List[Sir])(op)
}

split("12 2 3 4".toList)

