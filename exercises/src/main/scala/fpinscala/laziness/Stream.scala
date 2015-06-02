package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Empty => acc.reverse
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, Nil)
  }

  def toListNaive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
      case Empty => empty
      case _ if n < 1 => empty
      case Cons(h, t) => cons(h(), t().take(n - 1))
    }

  def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case _ if n < 1 => this
    case Cons(h, t) => t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons(_, t) => empty
  }

  def takeWhileFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, s) => if (p(a)) cons(a, s) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOptionFold: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, s) => cons(f(a), s))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, s) => if (p(a)) cons(a, s) else s)

  def append[B >: A](sb: => Stream[B]): Stream[B] =
    foldRight(sb)((a, s) => cons(a, s))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, sb) => f(a).append(sb))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this){
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)){
      case (Empty, _) => None
      case (Cons(h, t), i) if i > 0 => Some(h(), (t(), i - 1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) =>
        val a = h()
        if (p(a)) Some(a, t()) else None
    }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, bs)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(ah, at), Cons(bh, bt)) => Some(f(ah(), bh()), (at(), bt())) 
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, bs)) {
      case (Empty, Empty) => None
      case (Cons(ah, at), Empty) => Some((Some(ah()), None), (at(), Empty))
      case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (Empty, bt()))
      case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
    }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fib: Stream[Int] = {
    def fibFrom(i: Int, j: Int): Stream[Int] = cons(i, fibFrom(j, i + j))
    fibFrom(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, ss)) => cons(a, unfold(ss)(f))
  }

  def onesByUnfold: Stream[Int] = unfold(1)(i => Some((i, i)))

  def constantByUnfold[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))

  def fromByUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def fibByUnfold: Stream[Int] = unfold((0, 1)){ case (i, j) => Some(i, (j, i + j))}
}