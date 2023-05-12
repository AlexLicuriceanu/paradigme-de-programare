/*
def sumAll(start: Int, stop: Int): Int = {
  def loop (i: Int, acc: Int): Int =
    if (i > stop) acc
    else loop(i+1, i + acc)
  loop(start,0)
}

def sumSquares(start: Int, stop: Int): Int = {
  def loop (i: Int, acc: Int): Int =
    if (i > stop) acc
    else loop(i+1, i*i + acc)
  loop(start,0)
}*/

def sumWithF(f: Int => Int, start:Int, stop:Int): Int = {
  def loop (i: Int, acc: Int): Int =
    if (i > stop) acc
    else loop(i+1, f(i) + acc)
  loop(start,0)
}

def id(x: Int): Int = x
def square(x: Int): Int = x * x

sumWithF(id,0,10)
sumWithF(square,0, 10)

sumWithF((x: Int) => x, 0, 10)
sumWithF(x => x, 0, 10)
sumWithF(x => x * x, 0, 10)

val func: (Int,Int) => Int =
  (x: Int, y: Int) => x + y

val func2: (Int,Int) => Int  =
  (x,y) => x + y

/*
   Sa ne imaginam urmatorul scenariu:
   Avem un range extrem de mare de valori int,
   Vrem sa aplicam cativa algoritmi, pe foarte multe range-uri
   diferite.
   Algoritmii sunt putini (3), range-urile sunt foarte multe.

 */

sumWithF(x => x, 0, 10)
sumWithF(x => x * x, 10, 20)

def alg1(x: Int): Int = x
def alg2(x: Int): Int = x * x
def alg3(x: Int): Int = x * x * x

def currySumWithF(f: Int => Int): (Int, Int) => Int = {
  def sumWithF(start: Int, stop: Int): Int = {
    def loop(i: Int, acc: Int): Int =
      if (i > stop) acc
      else loop(i+1,f(i) + acc)
    loop(start,0)
  }
  sumWithF
}

def cleanSumWithF(f: Int => Int)(start: Int, stop: Int): Int = {
  def loop(i: Int, acc: Int): Int =
    if (i > stop) acc
    else loop(i+1, f(i) + acc)
  loop(start,0)
}


val applyAlg1:(Int, Int) => Int =
  cleanSumWithF(alg1)
val applyAlg2:(Int, Int) => Int =
  cleanSumWithF(alg2)

applyAlg1(0,10)
applyAlg2(0,10)

cleanSumWithF(x => x * x)(0,10)

val test: (Int, Int) => Int = cleanSumWithF(x => x * x)
test(0,10)

//primeste parametrii "pe rand" =
// functia este in forma curry
def f(x: Int)(y: Int)(z: Int): Int = x + y + z
val func3: Int => Int => Int = f(1)

f(1)(2)(3)

def g(x: Int, y: Int, z: Int): Int = x + y + z
g(1,2,3)

// cum procedam daca vrem sa compunem algoritmi?

//def compose(f: Int => Int, g: Int => Int): Int => Int =
//  x => f(g(x))

type Algorithm = Int => Int
def compose(f: Algorithm, g: Algorithm): Algorithm =
  x => f(g(x))

cleanSumWithF(compose(alg1,alg2))(0,10)

/*
Sa presupunem ca vrem sa definim functii 2D
y = ax + b
 */

type Fun2D = Int => Int

val ff: Int => Int = x => 2*x + 1

// in forma curry
def gen2DFun (a: Int)(b: Int): Fun2D =
  x => a*x + b

val fff = gen2DFun(2)(1)

def uncurry (gen: Int => Int => Fun2D): (Int,Int) => Fun2D =
  (x,y) => gen(x)(y)
