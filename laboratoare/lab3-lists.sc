import scala.annotation.tailrec

// 3.1.1
@tailrec
def atLeastk(k: Int, l: List[Int]): Boolean = {
  if (k == 0) true
  else {
    if (l == Nil) false
    else atLeastk(k-1, l.tail)
  }
}

atLeastk(6, List(1,2,3,4,5))

@tailrec
def atLeastkPred(pred: Int => Boolean)(k: Int, l: List[Int]): Boolean = {
  if (k == 0) true
  else {
    if (l == Nil) false
    else {
      if (pred(l.head)) atLeastkPred(pred)(k-1, l.tail)
      else atLeastkPred(pred)(k, l.tail)
    }
  }
}

atLeastkPred(x => x % 2 == 0)(2, List(1,2,3,4,5))

// 3.1.2
def take(n: Int, l: List[Int]): List[Int] = {
  (n, l) match {
    case (0, _) => Nil
    case (_, Nil) => Nil
    case (_, head::tail) => head::take(n-1, tail)
  }
}

take(3, List(1,2,3,4,5))

// 3.1.3
@tailrec
def drop(n: Int, l: List[Int]): List[Int] = {
  (n, l) match {
    case (0, _) => l
    case (_, Nil) => l
    case (_, _::tail) => drop(n-1, tail)
  }
}

drop(3, List(1,2,3,4,5))

// 3.1.4
def takeP(p: Int => Boolean)(l: List[Int]): List[Int] = {
  l match {
    case Nil => Nil
    case _ => {
      if (p(l.head)) l.head::takeP(p)(l.tail)
      else takeP(p)(l.tail)
    }
  }
}

takeP(_%2 == 0)(List(1,2,3,4,5,6))

// 3.1.5
def part(p: Int => Boolean)(l: List[Int]): (List[Int], List[Int]) = {
  l match {
    case Nil => (Nil, Nil)
    case head::tail => {
      val (trueList, falseList) = part(p)(tail)
      if (p(head)) (head::trueList, falseList)
      else (trueList, head::falseList)
    }
  }
}

part(_%2 == 0)(List(1,2,3,4,5,6))

type Str = List[Char]
val l: List[Str] = List("MAtei@gmail.com", "mihai@gmail.com", "tEst@mail.com", "email@email.com", "short@ax.ro").map(x => x.toList)

// 3.2.1
def remUpper(list: List[Str]): List[Str] = {
  list.map(email => email.filter(!_.isUpper))
}

remUpper(l)

// 3.2.2
def longer(k: Int, list: List[Str]): List[Str] = {
  list.filter(email => email.length <= k)
}

longer(14, l)

// 3.2.3
// sau list.count(_.length > k)
def howMany(k: Int)(list: List[Str]): Int = {
  list.foldRight(0)((email, count) => {
    if (email.length > k) count+1
    else count
  })
}

howMany(5)(l)

// 3.2.4
def namesEmails(list: List[Str]): List[(Str, Str)] = {
  def splitEmail(email: Str): (Str, Str) = {
    val (username, domain) = email.span(_ != '@')
    (username, domain.tail)
  }

  list.foldRight(Nil: List[(Str, Str)])(
    (email, acc) => {
      val (username, domain) = splitEmail(email)
      (username, domain)::acc
    })
}

namesEmails(l)

// 3.2.5
def domains(l: List[Str]): List[Str] = {
  l.foldRight(Nil: List[Str])(
    (domain, acc) => {
      if (!acc.contains(domain)) domain::acc
      else acc
    }
  )
}

val domainsList: List[Str] = List(List('g','m','a','i','l','.','c','o','m'), List('o','k','.','r','o'), List('g','m','a','i','l','.','c','o','m'))
domains(domainsList)

// 3.2.7
def domain(list: List[Str]): List[Str] = {
  list.map(mail => mail.span(_ != '.').head)
}

domain(domainsList)

type Gradebook = List[(Str, Int)]
val gradebook: Gradebook = List((List('G'), 3), (List('F'), 10), (List('M'), 6), (List('P'), 4), (List('A'), 2))

// 3.3.1
def increment(g: Gradebook): Gradebook = {
  g.map((name, grade) =>
    if (grade >= 5) (name, grade+1)
    else (name, grade))
}

increment(gradebook)

// 3.3.2
def average(g: Gradebook): Double = {
  val (sum, count) = g.foldRight(0, 0) {
    case ((_, grade), (s, c)) => (s+grade, c+1)
  }

  sum.toDouble / count.toDouble
}

average(gradebook)

// 3.3.3
def percentage(g: Gradebook): (Double,Double) = {
  val (failed, passed) = g.partition((_, grade) => grade <= 5)
  val total = g.size
  (100 * failed.size.toDouble/total.toDouble,
    100 * passed.size.toDouble/total.toDouble)
}

percentage(gradebook)

// 3.3.4
def pass(g: Gradebook): List[Str] = {
  g.filter((_, grade) => grade >= 5)
    .map((name, _) => name)
}

pass(gradebook)

// 3.3.5
def mergeSort(l: Gradebook): Gradebook = {
  def merge(u: Gradebook, v: Gradebook): Gradebook = ???
  ???
}

mergeSort(gradebook)

// 3.3.6
def honorsList(g: Gradebook): List[Str] = {
  mergeSort(g).map((name, _) => name)
}

honorsList(gradebook)


