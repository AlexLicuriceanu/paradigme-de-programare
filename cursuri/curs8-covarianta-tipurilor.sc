trait Nat {
  def +(n: Nat): Nat
  def -(n: Nat): Nat  // difference over naturals, not integers
}
 
case object Zero extends Nat {
  override def + (n: Nat): Nat = n
  override def - (n: Nat): Nat = Zero
}
 
case class Succ(n: Nat) extends Nat {
  override def + (m: Nat): Nat = Succ(n + m)
  override def - (m: Nat): Nat =
    m match {
      case Zero => Succ(n)
      case Succ(mp) => n - mp
    }
}
 
/*
 Vreau sa iau o lista de intregi (Int) si sa o transform
 intr-o lista de Nat DAR e posibil ca unii Int sa fie negativi
 vreau sa il ignor.
 */
 
//trait Result[A] // Option
//case class Error[A](msg: String) extends Result[A] //None
//case class Value[A](value: A) extends Result[A] //Some
 
 
def toNat(i: Int): Option[Nat] = {
  def fromValidInt(i: Int): Nat =
    if (i == 0) Zero
    else Succ(fromValidInt(i-1))
  if (i < 0) None
  else Some(fromValidInt(i))
}
 
toNat(-1)
toNat(2)
 
def fromIntList(l: List[Int]): List[Nat] =
  l.map(toNat)
    .foldRight(Nil:List[Nat])((x, acc) =>
                                x match {
                                  case None => acc
                                  case Some(n) => n :: acc
                                })
 
val m: Map[Int,Int] = Map (1 -> 2, 3 -> 4)
 
1 -> 2 // pereche
1.->(2)  // pereche
(1,2) // same as above
 
if (m.contains(0)) m(0) else -1
 
m.get(0) match{ //Option[Int]
  case None => -1
  case Some(x) => x
}
 
m.withDefaultValue(-1)(0)
 
/* Covarianta tipurilor */
 
// A,B,C
// T,U,V
 
trait Tree[+A] {
  def size: Int
  def map[B](f: A => B):Tree[B]
  def flatten:List[A]
}
 
// case object Void[A]
case class Void[A]() extends Tree[A] {
  override def size: Int = 0
  override def map[B](f: A => B):Tree[B] =
    Void()
  override def flatten: List[A] = Nil
}
 
case class Node[A](left: Tree[A], key: A, right:Tree[A]) extends Tree[A] {
  override def size: Int = 1 + left.size + right.size
  override def map[B](f: A => B): Tree[B] =
    Node(left.map(f),f(key),right.map(f))
 
  override def flatten: List[A] = left.flatten ++ List(key) ++ right.flatten
}
 
class Animal {
  def sing: String = "An animal sings"
}
 
class Dog extends Animal {
  override def sing: String = "Wof!"
}
 
class Cat extends Animal {
  override def sing: String = "Miau"
}
 
 
val catTree = Node(Void(),new Cat, Void())
val animalTree = Node(Void(), new Cat, Node(Void(), new Dog, Void()))
 
val tree = Node(Void(), new Dog, catTree)
 
def singTree(t: Tree[Animal]): String =
  t.map(_.sing).flatten.reduce(_ + _)
 
// Serialiser[A <: Animal] echivalent cu Serialiser<A extends Animal>
class Serialiser[-A <: Animal] {
  def serialise(a: A): String = a.sing
}
 
val animalSerialiser = new Serialiser[Animal]
val catSerialiser: Serialiser[Cat] = animalSerialiser
 
/*
   Ar trebui ca un Serialiser[Animal] sa fie subtip al Serialiser[Cat]
    daca stiu sa serializez un Cat, atunci stiu sa serializez si un Animal
    DA!
 
    Ar trebui ca Serialiser[Cat] sa fie subtip al Serialiser[Animal] ?
    NU!
 */
