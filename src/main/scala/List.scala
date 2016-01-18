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

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](lst: List[A]): List[A] = {
    if (List.tail(lst) == Nil || lst == Nil) {
      return Nil
    }
    lst match {
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  //this functions declares two generic types, A,B.question that i had for myself was why is type of input list
  // element different than the output type. applying a bit more pressure on System-2 revealed that:
  // It is always possible to have output type to be different because the list will not always be like List of Ints
  // that need to summed or List of doubles that need to be multiplied. It could also be a list of Person class
  // instances where an average age in the list needs to be computed.Keeping input list type and the output separate
  // allows that.
  def foldRight[A, B](as: List[A], z: B)
                     (f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)((x, y) => x * y)

  //Exercise 3.7
  //Can we introduce short circuiting in foldRight so that it returns 0 immediately when it encounters it.

  //Approach 1:
  //We could add a case in foldRight like Cons(0,xs) but the short coming of this approach is that we're now
  //hard coding that logic in foldRight that makes it more type dependent e.g. Double or Int. This would not
  //be a preferred approach

  //Approach 2:
  //Allow passing of another "short circuit value" into foldRight that allows such short circuit checking. If
  //we do that then adding a case statement like Case(ssValue,_) will allow us to directly return a 0 and halt
  //execution. This although has limitations, for example: in case of Ints we may not want to short circuit an
  //operation even if there is a 0 in the list. We could possibly add extra checks and guards in foldLeft but
  //that might kill elegance of the method as it stands currently.

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }


  //May be useless but an alternative implementation of foldLeft that i came up with
  //what probably fooled me here is that the name of the default argument. i got into thinking that
  //default can only be used as default value but not accumulator.
  def foldLeft_Confused[A, B](as: List[A], default: B)
                             (f: (B, A) => B): B = {
    @tailrec
    def go(as: List[A], default: B, acc: B): B = as match {
      case Nil => acc
      case Cons(x, xs) => go(xs, default, f(acc, x))
    }
    go(as, default, default)
  }


  //solution from fp scala git repo.
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)
                    (f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)

  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Int]) = foldLeft(ns, 1)((x, y) => x * y)

  def length3[A](as: List[A]): Int = {
    foldLeft(as, 0)((x, _) => x + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((x, y) => Cons(y, x))
  }

  def append[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)((head, tail) => Cons(head, tail))
  }
}