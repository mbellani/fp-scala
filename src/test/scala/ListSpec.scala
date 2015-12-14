import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {

  it should "produce 42" in {
    val value = List(1, 2, 3) match {
      case _ => 42
    }
    assert(value == 42)
  }

  it should "return a tail" in {
    val lst = List(1, 2, 3)
    val tail = List.tail(lst)
    assert(List.count(tail) == 2)
  }

  it should "throw an error for Nil.tail" in {
    intercept[RuntimeException] {
      List.tail(Nil)
    }
  }

  it should "return 3 when pattern matching" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
    }
    assert(x == 3)
  }

  it should " have new head" in {
    val lst = List(1, 2, 3)
    val head = List.setHead(lst, 9)
    assert(head.value.contains(9))
  }

  it should "drop elements" in {
    val lst = List(1, 2, 3)
    val dropped = List.drop(lst, 2)
    assert(dropped != lst)
    assert(List.count(dropped) == 1)
    assert(List.count(lst) == 3)
  }

  it should "should drop prefix element when predicate matches" in {
    val lst = List[Int](1, 2, 3)
    val n = List.dropWhile(lst, (v: Int) => (v == 1))
    assert(List.count(n) == 2)
  }

}
