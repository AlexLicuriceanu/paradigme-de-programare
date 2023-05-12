import scala.math._

case class Gradebook(book: Map[String,Int]) {
  // 5.1.1
  def +(entry: (String, Int)): Gradebook = {
    Gradebook(book + entry)
  }

  // 5.1.2
  def setGrade(name: String, newGrade: Int): Gradebook = {
    val newBook = book.updated(name, newGrade);
    Gradebook(newBook)
  }

  // 5.1.3
  def ++(other: Gradebook): Gradebook = {
    // the best strategy is to first implement the update of an entry into an existing Map...
    def updateBook(book: Map[String, Int], pair: (String, Int)): Map[String, Int] = {
      val name = pair._1
      val grade = pair._2

      book.getOrElse(name, -1) match {
        case -1 => book.updated(name, grade)
        case oldGrade => book.updated(name, math.max(oldGrade, grade))
      }
    }

    // and then use a fold to perform updates for all pairs of the current map.
    val newBook = other.book.foldLeft(book)(updateBook)
    Gradebook(newBook)
  }
}


val g = Gradebook(Map("s1" -> 3, "s2" -> 10, "s3" -> 7))

g + ("s4" -> 9)
g.setGrade("s1", 5)

val g1 = Gradebook(Map("s1" -> 3, "s2" -> 9))
val g2 = Gradebook(Map("s2" -> 10, "s3" -> 7))

g1 ++ g2

// 5.2.1
/*
trait Expr[A] {
  def eval(): A
}

case class BoolAtom(b: Boolean) extends Expr[Boolean] {
  override def eval(): Boolean = b
}
case class BoolAdd(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean] {
  override def eval(): Boolean = left.eval() || right.eval()
}
case class BoolMult(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean] {
  override def eval(): Boolean = left.eval() && right.eval()
}
*/

// 5.2.2
case class Strategy[A] (add: (A,A) => A, mult: (A,A) => A)

trait Expr[A] {
  def eval(s: Strategy[A]): A
}

case class BoolAtom(b: Boolean) extends Expr[Boolean] {
  override def eval(s: Strategy[Boolean]): Boolean = b
}

case class BoolAdd(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean] {
  override def eval(s: Strategy[Boolean]): Boolean = s.add(left.eval(s), right.eval(s))
}

case class BoolMult(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean] {
  override def eval(s: Strategy[Boolean]): Boolean = s.mult(left.eval(s), right.eval(s))
}

val boolStrategy = Strategy[Boolean](
  (left, right) => left || right,  // add implementation
  (left, right) => left && right   // mult implementation
)

val expr1 = BoolMult(BoolAdd(BoolAtom(true), BoolAtom(false)), BoolAtom(false))
val expr2 = BoolMult(BoolAdd(BoolAtom(true), BoolAtom(false)), BoolAtom(true))

println(expr1.eval(boolStrategy))  // false
println(expr2.eval(boolStrategy))  // true


// 5.2.3
case class Atom[A](a: A) extends Expr[A] {
  override def eval(f: Strategy[A]) = a
}

case class Add[A](left: Expr[A], right: Expr[A]) extends Expr[A] {
  override def eval(f: Strategy[A]): A = f.add(left.eval(f), right.eval(f))
}

case class Mult[A](left: Expr[A], right: Expr[A]) extends Expr[A] {
  override def eval(f: Strategy[A]): A = f.mult(left.eval(f), right.eval(f))
}

// 5.2.4


//
case class Polynomial (terms: Map[Int,Int]) {
  override def toString: String = {
    def printRule(x: (Int, Int)): String = x match {
      case (0, coeff) => coeff.toString
      case (1, coeff) => coeff.toString ++ "*x"
      case (p, coeff) => coeff.toString ++ "*x^" ++ p.toString
    }

    terms.toList.sortWith(_._1 >= _._1)
      .map(printRule)
      .reduce(_ ++ " + " ++ _)
  }

  // 5.3.1
  def * (n: Int): Polynomial = {
    Polynomial(terms.map((pow, coeff) => pow -> (coeff * n)))
  }

  // 5.3.3
  def + (p2: Polynomial): Polynomial = {
    val keys = terms.keySet ++ p2.terms.keySet
    val mergedTerms = keys.map(k => k -> (terms.getOrElse(k, 0) + p2.terms.getOrElse(k, 0)))
    Polynomial(mergedTerms.toMap)
  }

  // 5.3.4
  def * (p2: Polynomial): Polynomial = {
    ???
  }

  // 5.3.2
  def hasRoot(r: Int): Boolean = {
    terms.foldLeft(0) { case (acc, (exp, coeff)) => acc + (coeff * math.pow(r, exp)).toInt } == 0
  }
}

val p = Polynomial(Map(2 -> 1, 1 -> 2, 0 -> 1))  // encodes x^2 + 2*x + 1
p.toString()

(p * 3).toString

p.hasRoot(-2)

val p1 = Polynomial(Map(2 -> 1, 1 -> 2, 0 -> 0))
val p2 = Polynomial(Map(3 -> 2, 2 -> 3, 1 -> 2, 0 -> 1))

p1 + p2