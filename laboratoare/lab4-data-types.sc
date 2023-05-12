import scala.math._

trait NaturalNumber
case object Zero extends NaturalNumber
case class Successor(x: NaturalNumber) extends NaturalNumber

// 4.1.1
def add(x: NaturalNumber, y: NaturalNumber): NaturalNumber = {
  x match {
    case Zero => y
    case Successor(xs) => Successor(add(xs, y))
  }
}

add(Successor(Successor(Zero)), Successor(Successor(Zero)))

// 4.1.2
def multiply(x: NaturalNumber, y: NaturalNumber): NaturalNumber = {
  x match {
    case Zero => y
    case Successor(xs) => add(multiply(y, xs), y)
  }
}

multiply(Successor(Successor(Successor(Zero))),
  Successor(Successor(Zero)))


// 4.1.3
def toNaturalNumber(x: Int): NaturalNumber = {
  x match {
    case 0 => Zero
    case xs => Successor(toNaturalNumber(xs-1))
  }
}

toNaturalNumber(4)

trait BTree
case object EmptyTree extends BTree
case class Node(value: Int, left: BTree, right: BTree) extends BTree

val testTree: BTree = Node(1,
  Node(7,
    Node(5, EmptyTree, EmptyTree),
    Node(8, EmptyTree, EmptyTree)
  ),

  Node(9,
    Node(11,
      Node(4, EmptyTree, EmptyTree),
      Node(12, EmptyTree, EmptyTree)),
    EmptyTree
  )
)
// 4.2.1
def depth(tree: BTree): Int = {
  tree match {
    case EmptyTree => -1
    case Node(value, left, right) =>
      max(depth(left), depth(right)) + 1
  }
}

depth(testTree)

// 4.2.2
def evenChildCount(tree: BTree): Int = {
  tree match {
    case EmptyTree => 0
    case Node(_, EmptyTree, EmptyTree) => 0
    case Node(_, left, right) => {
      (left, right) match {
        case (Node(_, l1, r1), Node(_, l2, r2)) =>
          1 + evenChildCount(left) + evenChildCount(right)
        case _ => evenChildCount(left) + evenChildCount(right)
      }
    }
  }
}

evenChildCount(testTree)

// 4.2.3
def flatten(tree: BTree): List[Int] = {
  tree match {
    case EmptyTree => Nil
    case Node(value, left, right) => {
      List(value) ++ flatten(left) ++ flatten(right)
    }
  }
}

flatten(testTree)

// 4.2.4
def countNodes(tree: BTree, cond: Int => Boolean): Int = {
  tree match {
    case EmptyTree => 0
    case Node(value, left, right) => {
      if (cond(value)) 1 + countNodes(left, cond) + countNodes(right, cond)
      else countNodes(left, cond) + countNodes(right, cond)
    }
  }
}

countNodes(testTree, x => x % 2 == 1)

// 4.2.5
def mirror(tree: BTree): BTree= {
   tree match {
     case EmptyTree => EmptyTree
     case Node(value, left, right) => Node(value, mirror(right), mirror(left))
  }
}

mirror(testTree)

type Matrix = List[List[Int]]
val testMatrix: Matrix = List(
  List(1,2,3),
  List(4,5,6),
  List(7,8,9)
)

val m1: Matrix = List(
  List(1,2),
  List(3,4)
)

val m2: Matrix = List(
  List(5,6),
  List(7,8)
)

// 4.3.1
def vprod(m: Matrix)(v: Int): Matrix = {
  m.map(_.map(_ * v))
}

vprod(testMatrix)(2)

// 4.3.2
def join(m1: Matrix, m2: Matrix): Matrix = {
  m1.zip(m2).map(_ ++ _)
}

join(m1, m2)

// 4.3.3
def vjoin(m1: Matrix, m2: Matrix): Matrix = {
  m1 ++ m2
}

vjoin(m1, m2)

// 4.3.4
def msum(m1: Matrix, m2: Matrix): Matrix = {
  m1.zip(m2).map(_.zip(_).map((x, y) => x + y))
}

msum(m1, m2)

// 4.3.5
def tr(m: Matrix): Matrix = {
  m match {
    case Nil :: _ => Nil
    case _ => m.map(_.head) :: tr(m.map(_.tail))
  }
}

tr(testMatrix)

// 4.3.6
def mprod(m1: Matrix, m2: Matrix): Matrix = {
???
}