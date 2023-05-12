/*
  Ce facem azi? - diverse
 
 */
 
// Ranges
1.to(10)
val f: (Int,Int) => Range =
  (start,stop) => start.to(stop)
 
1.to(10) // nu neaparat natural
1 to 10 // acelasi lucru
 
/*
mai general:
<object>.<method>(<p1>, ... <pn>)
<object> <method> <p1> <p2> .... <pn>
 */
 
1 until 10
1 to 10 by 2
 
trait Nat {
  def +(other: Nat): Nat
  def -(other: Nat): Nat
}
 
case object Zero extends Nat {
  override def + (other: Nat): Nat = other
  override def - (other: Nat): Nat = this
}
 
case class Succ(n: Nat) extends Nat {
  override def + (other: Nat): Nat = Succ(n + other)
  override def - (other: Nat): Nat =
    other match {
      case Zero => this
      case Succ(m) => n - m
    }
}
 
def fromInteger(i: Int): Nat =
  if (i == 0) Zero
  else Succ(fromInteger(i-1))
 
def fromString(s: String): Nat =
  fromInteger(s.toInt)
 
def fromList(l: List[Char]): Nat =
  fromString(l.mkString)
 
 
Succ(Zero) + fromInteger(10) + fromString("9")
 
//companion objects
object Nat {
 
  def apply(i: Int): Nat =
    if (i == 0) Zero
    else Succ(Nat(i-1))
 
  def apply(s: String): Nat =
    Nat(s.toInt)
}
Nat("2") + Nat(1)
 
/*
    Companion object - acelasi nume cu clasa
    SI
    metode numite mereu apply
 
    Fiecare case class are propria
    metoda apply definita by default
 */
 
case class Test(x: Int)
 
object Test {
  def apply(x: Int): Test = {
    println("Companion")
    Test(1)
  }
}
 
val m1 = List(List(1,2,3),List(4,5,6),List(7,8,9))
val transposedm2 = List(List(1,0,0),List(0,1,0),List(0,0,1))
 
List(1,2,3).map(_*2)
 
for (x <- List(1,2,3))
  yield x*2
 
List(1,2,3,4)
  .filter(_ % 2 == 0)
  .map(_+1)
 
for (x <- List(1,2,3,4) if x % 2 == 0)
  yield x+1
 
m1.map(_.map(_*2))
 
for (line <- m1)
  yield
    for (elem <- line)
      yield 2*elem
 
for (line <- m1;
     elem <- line)
  yield 2*elem
 
// produs cartezian
 
for (x <- List(1,2,3);
     y <- List(1,2,3))
  yield (x,y)
 
for (line <- m1)
  yield
    for (col <- transposedm2)
      yield line
              .zip(col)
              .map(p => p._1 * p._2)
              .foldRight(0)(_ + _)