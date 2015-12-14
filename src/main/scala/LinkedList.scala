import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) return Nil
    else Cons(as.head, apply(as.tail: _*))
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

  def dropWhile[A](l: List[A],
                   f: A => Boolean): List[A] = {

  }
}

object M {
  def main(args: Array[String]) {
    //    println(List(1, 2, 3) match { case _ => 42 })
    //    println(List(1, 2, 3) match { case Cons(_, t) => t })
    //
    //    val x = List(1, 2, 3, 4, 5) match {
    //      case Cons(x, Cons(2, Cons(4, _))) => x
    //      case Nil => 42
    //      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    //      case Cons(h, t) => h + List.sum(t)
    //    }
    //    println(x)

    val lst = List(1, 2, 3)

    //get tail

    //    println(List.tail(lst))
    //    println(List.tail(Nil))

    // set a new head

    //    val new_head = List.setHead(lst, 9)
    //    println(new_head)

    //drop elements from the list
    //    val dropped = List.drop(lst, 2)
    //    println(dropped)
    //    println(lst)


  }
}
