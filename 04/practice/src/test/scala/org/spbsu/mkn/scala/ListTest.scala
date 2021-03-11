package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.MyList.{fromSeq, size, sum}
import ListComparator.listComparator

class ListTest extends AnyFunSuite {
  test("head") {
    assert(fromSeq(Seq(1, 2, 3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1, 2, 3)).tail == fromSeq(Seq(2, 3)))
    assert(fromSeq(Seq(1)).tail == Nil)
  }

  test("drop") {
    assert(fromSeq(Seq(1, 2, 3)).drop(0) == fromSeq(Seq(1, 2, 3)))
    assert(fromSeq(Seq(1, 2, 3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1, 2, 3)).drop(3) == Nil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1, 2, 3)).drop(10))
  }

  test("take") {
    assert(fromSeq(Seq(1, 2, 3)).take(0) == Nil)
    assert(fromSeq(Seq(1, 2, 3)).take(2) == fromSeq(Seq(1, 2)))
    assert(fromSeq(Seq(1, 2, 3)).take(3) == fromSeq(Seq(1, 2, 3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1, 2, 3)).take(10))
  }

  test("map") {
    assert((Nil: MyList[Int]).map(x => x * 2) == Nil)
    assert(fromSeq(Seq(1, 2, 3)).map(_ * 2) == fromSeq(Seq(2, 4, 6)))
    assert(fromSeq(Seq(1, 2, 3)).map(identity) == fromSeq(Seq(1, 2, 3)))
  }

  test("size") {
    assert(size(Nil) == 0)
    assert(size(fromSeq(Seq(1, 2, 3))) == 3)
  }

  test("sum") {
    assertThrows[UnsupportedOperationException](sum(Nil))
    assert(sum(fromSeq(Seq(1, 2, 3))) == 6)
    assert(sum(fromSeq(Seq(1))) == 1)
  }


  test("sort") {
    assert(MyList.sort(fromSeq(Seq(26, 35, 44, 13, 62, 11)))((x: Int, y: Int) => x % 10 - y % 10) ==
      fromSeq(Seq(11, 62, 13, 44, 35, 26)))
    assert(MyList.sort(fromSeq(Seq("b", "aa", "aaa", "a")))((x: String, y: String) => x.length - y.length) ==
      fromSeq(Seq("b", "a", "aa", "aaa")))

    assert(MyList.sort(fromSeq(Seq(3, 2, 1))) == MyList.fromSeq(Seq(1, 2, 3)))
    assert(MyList.sort(fromSeq(Seq("c", "b", "a"))) == MyList.fromSeq(Seq("a", "b", "c")))

    assert(
      MyList.sort(
        fromSeq(
          Seq(
            fromSeq(Seq(2)),
            fromSeq(Seq(1, 2)),
            fromSeq(Seq(1, 3)),
            fromSeq(Seq(1, 2, 3)),
            fromSeq(Seq(1, 2, 2))
          )
        )
      ) ==
        fromSeq(
          Seq(
            fromSeq(Seq(1, 2)),
            fromSeq(Seq(1, 2, 2)),
            fromSeq(Seq(1, 2, 3)),
            fromSeq(Seq(1, 3)),
            fromSeq(Seq(2))
          )
        )
    )
  }
}
