/*
Avem un sir, care contine numere si whitespace.
  "12 4 560 3"
 
 Vrem sa realizam un split dupa whitespace al acestui sir:
 "12", "4", "560", "3"
 */
type Sir = List[Char]
 
 
 
 
/*
Haide sa parsam un CSV.
a,b,c
d,e,f
g,h,i
 */
 
 
val csvContents: Sir =
  """a,b,c
    |d,e,f
    |g,h,i
    |""".stripMargin.toList
 
type Table = List[List[Sir]]
 
//split('\n')(csvContents)
 
//split('\n')(csvContents)
//  .map(split(','))
 
def readCSV(content: Sir): Table = {
  def split(delim: Char)(s: Sir): List[Sir] = {
    def op (c: Char, acc: List[Sir]): List[Sir] =
      acc match {
        case Nil => if (c == delim) Nil else List(List(c))
        case x :: xs => if (c == delim) Nil :: acc else (c :: x) :: xs
      }
    s.foldRight(Nil: List[Sir])(op)
  }
 
  split('\n')(content)
    .map(split(','))
}
 
readCSV(csvContents)
 
def writeCSV(t: Table): Sir = {
  def op(delim: Char)(e: Sir, acc: Sir): Sir =
    acc match {
      case Nil => e
      case _ => e ++ List(delim)++acc
    }
 
  t.foldRight(Nil:Sir)((line, acc) => line.foldRight(Nil:Sir)(op(',')) ++ List('\n') ++ acc )
//  t.map(line => line.foldRight(Nil:Sir)(op(',')))
//    .foldRight(Nil:Sir)(op('\n'))
 
 
}
 
 
val f: (Int,Int) => Int = (x, y) => x + y
val g: (Int,Int) => Int = _ + _
 
/*
  Vrem sa citim, in loc de un fisier CSV, o matrice de Int
 
 */
 
type Matrix = List[List[Int]]
type Tabular[A] = List[List[A]]
 
def readTabular[A](read:Sir => A)(content: Sir): Tabular[A] = {
  def split(delim: Char)(s: Sir): List[Sir] = {
    def op (c: Char, acc: List[Sir]): List[Sir] =
      acc match {
        case Nil => if (c == delim) Nil else List(List(c))
        case x :: xs => if (c == delim) Nil :: acc else (c :: x) :: xs
      }
    s.foldRight(Nil: List[Sir])(op)
  }
 
  split('\n')(content)
    .map((split(',')(_)).andThen(_.map(read(_))))
 
}
 
val m =
  """1,2,3
    |4,5,6
    |7,8,9
    |""".stripMargin.toList
 
val m1: Tabular[Int] = readTabular(_.foldRight("")(_ + _).toInt)(m)
 
m1.map(_.map(_*2))
 
def transpose(m: Matrix): Matrix =
  m match{
    case Nil :: _ => Nil
    case _ => m.map(_.head) :: transpose(m.map(_.tail))
  }
 
 
List(1,2,3).zip(List(1,0,0))
  .map(pair => pair._1 * pair._2)
  .foldRight(0)(_ + _)
/*
    1 2 3    1 0 0  (deja transpusa)
    4 5 6    1 1 0
    7 8 9    0 1 0
 
vreau sa calculez prima linie din matricea produs:
 */
List(List(1,0,0),List(1,1,0),List(0,1,0))
  .map(List(1,2,3).zip(_).map(pair => pair._1 * pair._2).sum)
 
/*
  daca vreau sa construiesc matricea produs:
 */
val transposed = List(List(1,0,0),List(1,1,0),List(0,1,0))
 
List(List(1,2,3),List(4,5,6),List(7,8,9)).
  map(line => transposed.map(line.zip(_).map(pair => pair._1 * pair._2).sum))
 
def product(m1: Matrix, m2: Matrix): Matrix =
  m1.map(
    line => transpose(m2).map(
      col => line.zip(col).map(pair => pair._1 * pair._2).foldRight(0)(_ + _)))
 
val m1 = List(List(1,2,3),List(4,5,6),List(7,8,9))
val m2 = List(List(1,0,0),List(0,1,0),List(0,0,1))
product(m1,m2)