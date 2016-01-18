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

  it should "drop the last element from List" in {
    val lst = List.init(List(1, 2, 3, 4))
    assert(List.count(lst) == 3)
  }

  it should "be a surprise when we pass Nil and Cons themselves to foldRight" in {
    val value = List.foldRight(List(1, 2, 3, 4), Nil: List[Int])((Cons(_, _)))
    println(value)
  }

  it should "return length of 3" in {
    val lst = List(1, 2, 3)
    assert(List.length(lst) == 3)
  }

  it should "sum numbers correctly with foldLeft" in {
    val lst = List(1, 2, 3)
    assert(List.sum3(lst) == 6)
  }

  it should "multiply numbers correctly with foldRight" in {
    val lst = List(1, 2, 3, 4)
    assert(List.product3(lst) == 24)
  }

  it should "return length when using foldLeft in length3" in {
    val lst = List(1, 2, 3, 4)
    assert(List.length3(lst) == 4)
  }

  it should "return a reverse list" in {
    val lst = List(1, 2, 3, 4)
    println("reversed" + List.reverse(lst))

  }

  it should "should append to list" in {
    val lst1 = List(1, 2, 3, 4)
    val lst2 = List(5, 6, 7)
    val lst = List.append(lst1, lst2)
    println("appended " + lst)
    assert(List.length3(lst) == 7)

  }

}
