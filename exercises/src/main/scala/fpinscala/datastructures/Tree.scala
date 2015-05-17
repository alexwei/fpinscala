package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = {
    @annotation.tailrec
    def go(ts: List[Tree[A]], acc: Int): Int = ts match {
      case Nil => acc
      case Cons(Leaf(_), tail) => go(tail, acc + 1)
      case Cons(Branch(l, r), tail) => go(Cons(l, Cons(r, tail)), acc + 1)
    }
    go(List(tree), 0)
  }


}