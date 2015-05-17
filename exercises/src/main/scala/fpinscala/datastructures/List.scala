package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons.apply)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
  {
    @annotation.tailrec
    def go(l: List[A], g: B => B): B = l match {
      case Nil => g(z)
      case Cons(h, t) => go(t, b => g(f(h, b)))
    }
    go(as, b => b)
  }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ if n <= 0 => l
    case Nil => Nil
    case Cons(_, t) => drop(t, n - 1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, a) => Cons(a, acc))

  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(list: List[A], acc: List[A]): List[A] = list match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => reverse(acc)
      case Cons(h, t) => go(t, Cons(h, acc))
    }
    go(l, Nil)
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldLeftByRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldRightByLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldLeftByRightNoReverse[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightByLeftNoReverse[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def concat[A](ls: List[List[A]]): List[A] = foldRight(ls, List[A]())(append)

  def add1(l: List[Int]): List[Int] = map(l)(_ + 1)

  def double2String(l: List[Double]): List[String] = map(l)(_.toString)

  def filter[A](l: List[A])(p: A => Boolean): List[A] = foldRight(l, List[A]())((a, acc) => if (p(a)) Cons(a, acc) else acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, List[B]())((a, acc) => append(f(a), acc))

  def filterViaFlatMap[A](l: List[A])(p: A => Boolean): List[A] = flatMap(l)(a => if (p(a)) List(a) else Nil)

  def addList(as: List[Int], bs: List[Int]): List[Int] =
    reverse(foldLeft(as, (bs, List[Int]())){
      case ((Cons(bh, bt), acc), a) => (bt, Cons(a + bh, acc))
      case ((Nil, acc), _) => (Nil, acc)
    }._2)

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def go(al: List[A], bl: List[B], g: List[C] => List[C]): List[C] = (al, bl) match {
      case (Nil, _) => g(Nil)
      case (_, Nil) => g(Nil)
      case (Cons(a, at), Cons(b, bt)) => go(at, bt, acc => g(Cons(f(a, b), acc)))
    }
    go(as, bs, cs => cs)
  }

  @annotation.tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    if (startsWith(sup, sub)) true
    else sup match {
      case Nil => false
      case Cons(h, t) => hasSubsequence(t, sub)
    }
}
