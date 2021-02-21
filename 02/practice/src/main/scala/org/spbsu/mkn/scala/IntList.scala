package org.spbsu.mkn.scala

// extraTask
sealed trait Foldable[A] {
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
    case Cons(x, IntNil) => x
    case Cons(x, xs) => x + IntList.sum(xs)
  }

  def size(intList: IntList): Int = intList match {
    case IntNil => 0
    case Cons(_, xs) => 1 + IntList.size(xs)
  }

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

case class Cons(x: Int, xs: IntList) extends IntList {
  override def head: Int = x

  override def tail: IntList = xs

  override def drop(n: Int): IntList = if (n == 0) {
    Cons(x, xs)
  } else {
    xs.drop(n - 1)
  }

  override def take(n: Int): IntList = if (n == 0) {
    IntNil
  } else {
    Cons(x, xs.take(n - 1))
  }

  override def map(f: Int => Int): IntList = Cons(f(x), xs.map(f))

  override def foldLeft[B](init: B)(op: (B, Int) => B): B = op(xs.foldLeft(init)(op), x)
}