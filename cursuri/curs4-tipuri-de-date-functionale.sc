trait Nat

//case class Zero() extends Nat
case object Zero extends Nat    // singleton
case class Succ(next: Nat) extends Nat

Succ(Succ(Succ(Zero)))
Succ(Zero) == Succ(Zero)    // true

// Descompunere in functional: pattern matching
def add(n: Nat, m: Nat): Nat =
  n match {
    case Zero => m
    case Succ(np) => Succ(add(np, m))
  }

add(Succ(Zero), Succ(Zero))

def equals(n: Nat, m: Nat): Boolean = {
  (n, m) match {
    case (Zero, Zero) => true
    case (Succ(x), Succ(y)) => equals(x, y)
    case _ => false
  }
}

trait IList {
  def append(other: IList): IList
  def size(): Int
  def take(n: Int): IList
  def drop(n: Int): IList
  def merge(other: IList): IList
}

case object Void extends IList {
  override def size(): Int = 0
  override def append(other: IList): IList = other
  override def take(n: Int): IList = Void
  override def drop(n: Int): IList = Void
  override def merge(other: IList): IList = other
}

case class Cons(h: Int, t: IList) extends IList {
  override def size(): Int = 1 + t.size()
  override def append(other: IList): IList = Cons(h, t.append(other))
  override def take(n: Int): IList =
    if (n == 0) Void
    else Cons(h, t.take(n-1))

  override def drop(n: Int): IList =
    if (n == 0) this
    else t.drop(n-1)

  override def merge(other: IList): IList =
    other match {
      case Void => ???
      case Cons(x, xs) => ???
    }
}

/*
  Adaugarea de operatii:
    Functional - usor
    OOP - dificil

  Adaugarea de constructori:
    Functional - dificil
    OOP - usor
 */

def merge(l1: IList, l2: IList): IList =
  (l1, l2) match {
    case (Void, _) => l2
    case (_, Void) => l1
    case (Cons(x, xs), Cons(y, ys)) =>
      if (x > y) Cons(y, merge(l1, ys))
      else Cons(x, merge(xs, l2))
  }

/*
  Daca avem de implementat o operatie locala, o putem face
  cu usurinta intr-un membru de clasa.

  Daca avem nevoie de informatii din afara clasei e de preferat
  abordarea folosind pattern matching
  (Append este local pt ca nu imi pasa de cealalta lista)
 */
