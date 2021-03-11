package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.List.{fromSeq, size, sum}

class ListTest extends AnyFunSuite {
  test("head") {
    assert(fromSeq(Seq(1,2,3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1,2,3)).tail == fromSeq(Seq(2,3)))
    assert(fromSeq(Seq(1)).tail == Nil)
  }

  test("drop") {
    assert(fromSeq(Seq(1,2,3)).drop(0) == fromSeq(Seq(1,2,3)))
    assert(fromSeq(Seq(1,2,3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1,2,3)).drop(3) == Nil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).drop(10))
  }

  test("take") {
    assert(fromSeq(Seq(1,2,3)).take(0) == Nil)
    assert(fromSeq(Seq(1,2,3)).take(2) == fromSeq(Seq(1,2)))
    assert(fromSeq(Seq(1,2,3)).take(3) == fromSeq(Seq(1,2,3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).take(10))
  }

  test("map") {
    assert(Nil.map[Int, Int](_ * 2) == Nil)
    assert(fromSeq(Seq(1,2,3)).map[Int, Int](_ * 2) == fromSeq(Seq(2,4,6)))
    assert(fromSeq(Seq(1,2,3)).map[Int, Int](identity) == fromSeq(Seq(1,2,3)))
  }

  test("size") {
    assert(size(Nil) == 0)
    assert(size(fromSeq(Seq(1,2,3))) == 3)
  }

  test("sum") {
    assertThrows[UnsupportedOperationException](sum(Nil))
    assert(sum(fromSeq(Seq(1,2,3))) == 6)
    assert(sum(fromSeq(Seq(1))) == 1)
  }

}
