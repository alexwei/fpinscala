package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  val tree =
    Branch(
      Branch(
        Branch(
          Branch(
            Leaf(1),
            Leaf(2)
          ),
          Leaf(3)
        ),
        Branch(
          Leaf(4),
          Branch(
            Leaf(5),
            Leaf(6)
          )
        )
      ),
      Branch(
        Leaf(7),
        Branch(
          Leaf(8),
          Branch(
            Leaf(9),
            Branch(
              Leaf(10),
              Branch(
                Leaf(11),
                Leaf(12)
              )
            )
          )
        )
      )
    )

  def size[A](tree: Tree[A]): Int = {
    @annotation.tailrec
    def go(ts: List[Tree[A]], acc: Int): Int = ts match {
      case Nil => acc
      case Cons(Leaf(_), tail) => go(tail, acc + 1)
      case Cons(Branch(l, r), tail) => go(Cons(l, Cons(r, tail)), acc + 1)
    }
    go(List(tree), 0)
  }

  def maximum(tree: Tree[Int]): Int = {
    @annotation.tailrec
    def go(h: Tree[Int], ts: List[Tree[Int]], result: Option[Int]): Int = (h, ts) match {
      case (Leaf(i), Nil) => result.getOrElse(i) max i
      case (Leaf(i), Cons(t, tt)) => go(t, tt, Some(result.getOrElse(i) max i))
      case (Branch(l, r), _) => go(l, Cons(r, ts), result)
    }
    go(tree, Nil, None)
  }

  def depth[A](tree: Tree[A]): Int = {
    @annotation.tailrec
    def go(h: (Tree[A], Int), ts: List[(Tree[A], Int)], result: Option[Int]): Int = (h, ts) match {
      case ((Leaf(_), d), Nil) => result.getOrElse(d) max d
      case ((Leaf(_), d), Cons(t, tt)) => go(t, tt, Some(result.getOrElse(d) max d))
      case ((Branch(l, r), d), _) => go((l, d + 1), Cons((r, d + 1), ts), result)
    }
    go((tree, 0), Nil, None)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    @annotation.tailrec
    def go(h: Either[Tree[B], Tree[A]], ts: List[Either[Tree[B], Tree[A]]]): Tree[B] = (h, ts) match {
      case (Right(Leaf(a)), Nil) => Leaf(f(a))
      case (Right(Leaf(a)), Cons(Right(ta), rest)) => go(Right(ta), Cons(Left(Leaf(f(a))), rest))
      case (Right(Leaf(a)), Cons(Left(tb), rest)) => go(Left(Branch(tb, Leaf(f(a)))), rest)
      case (Right(Branch(la, ra)), rest) => go(Right(la), Cons(Right(ra), rest))
      case (Left(tb), Nil) => tb
      case (Left(tb), Cons(Right(ta), rest)) => go(Right(ta), Cons(Left(tb), rest))
      case (Left(tb), Cons(Left(tb1), rest)) => go(Left(Branch(tb1, tb)), rest)
    }
    go(Right(tree), Nil)
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    @annotation.tailrec
    def go(h: Either[B, Tree[A]], ts: List[Either[B, Tree[A]]]): B = (h, ts) match {
      case (Right(Leaf(a)), Nil) => f(a)
      case (Right(Leaf(a)), Cons(Right(ta), rest)) => go(Right(ta), Cons(Left(f(a)), rest))
      case (Right(Leaf(a)), Cons(Left(b), rest)) => go(Left(g(b, f(a))), rest)
      case (Right(Branch(la, ra)), rest) => go(Right(la), Cons(Right(ra), rest))
      case (Left(b), Nil) => b
      case (Left(b), Cons(Right(ta), rest)) => go(Right(ta), Cons(Left(b), rest))
      case (Left(b1), Cons(Left(b0), rest)) => go(Left(g(b0, b1)), rest)
    }
    go(Right(tree), Nil)
  }

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)(_ + 1 max _ + 1)

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}