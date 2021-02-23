package org.spbsu.mkn.scala

// extraTask
trait Foldable[A] {
  def foldLeft[B](init: B)(op: (B, A) => B): B
}

sealed trait IntList extends Foldable[Int] {
  def head: Int

  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList = Cons(elem, this)
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = seq.foldRight[IntList](IntNil) { (x, xs) => Cons(x, xs) }

  def sum(intList: IntList): Int = intList match {
    case IntNil => undef
    case lst => sumWithFoldLeft(lst)
  }

  def size(intList: IntList): Int = intList.foldLeft(0)((sz, _) => sz + 1)

  // extra task: implement sum using foldLeft

  def sumWithFoldLeft(intList: IntList): Int = intList.foldLeft(0)((x, sum) => x + sum)
}

case object IntNil extends IntList {
  override def head: Int = IntList.undef

  override def tail: IntList = IntList.undef

  override def drop(n: Int): IntList = if (n == 0) {
    IntNil
  } else {
    IntList.undef
  }

  override def take(n: Int): IntList = if (n == 0) {
    IntNil
  } else {
    IntList.undef
  }

  override def map(f: Int => Int): IntList = IntNil

  override def foldLeft[B](init: B)(op: (B, Int) => B): B = init
}

case class Cons(head: Int, tail: IntList) extends IntList {

  override def drop(n: Int): IntList = if (n < 0) {
    IntList.undef
  } else if (n == 0) {
    Cons(head, tail)
  } else {
    tail.drop(n - 1)
  }

  override def take(n: Int): IntList = if (n < 0) {
    IntList.undef
  } else if (n == 0) {
    IntNil
  }
  else {
    Cons(head, tail.take(n - 1))
  }

  override def map(f: Int => Int): IntList = Cons(f(head), tail.map(f))

  override def foldLeft[B](init: B)(op: (B, Int) => B): B = op(tail.foldLeft(init)(op), head)
}