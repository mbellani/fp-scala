import scala.annotation.tailrec

sealed trait List[+A] {
  def value: Option[A];
}

case object Nil extends List[Nothing] {
  def value = None
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  def value = Some(head)
}

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) return Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("can't set a nil for head")
    case Cons(_, t) => Cons(h, t)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0 || l == Nil) {
      return l
    }
    l match {
      case Nil => l
      case Cons(_, tail) => drop(tail, n - 1)
    }
  }

  def count[A](lst: List[A]): Int = {
    @tailrec
    def go(lst: List[A], count: Int): Int = lst match {
      case Nil => count
      case Cons(_, tail) => go(tail, count + 1)

    }
    go(lst, 0)
  }
}
