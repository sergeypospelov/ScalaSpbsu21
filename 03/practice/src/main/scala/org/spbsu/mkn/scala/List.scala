package org.spbsu.mkn.scala

// extraTask
trait Foldable[+A] {
  def foldLeft[B](init: B)(op: (B, A) => B): B
}

sealed trait List[+A] extends Foldable[A] {
  def head: A

  def tail: List[A]

  def drop(n: Int): List[A]

  def take(n: Int): List[A]

  def map[C >: A, B](f: C => B): List[B]

  def ::[B >: A](elem: B): List[B] = Cons(elem, this)
}

object List {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[A](seq: Seq[A]): List[A] = seq.foldRight[List[A]](Nil) { (x, xs) => Cons(x, xs) }

  def sum(intList: List[Int]): Int = intList match {
    case Nil => undef
    case lst => sumWithFoldLeft(lst)
  }

  def size[A](intList: List[A]): Int = intList.foldLeft(0)((sz, _) => sz + 1)

  // extra task: implement sum using foldLeft

  def sumWithFoldLeft(intList: List[Int]): Int = intList.foldLeft(0)((x, sum) => x + sum)
}

case object Nil extends List[Nothing] {
  override def head: Nothing = List.undef

  override def tail: List[Nothing] = List.undef

  override def drop(n: Int): List[Nothing] = if (n == 0) {
    Nil
  } else {
    List.undef
  }

  override def take(n: Int): List[Nothing] = if (n == 0) {
    Nil
  } else {
    List.undef
  }

  override def map[C, B](f: C => B): List[Nothing] = Nil

  override def foldLeft[B](init: B)(op: (B, Nothing) => B): B = init
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {

  override def drop(n: Int): List[A] = if (n < 0) {
    List.undef
  } else if (n == 0) {
    Cons(head, tail)
  } else {
    tail.drop(n - 1)
  }

  override def take(n: Int): List[A] = if (n < 0) {
    List.undef
  } else if (n == 0) {
    Nil
  }
  else {
    Cons(head, tail.take(n - 1))
  }

  override def map[C >: A, B](f: C => B): List[B] = Cons(f(head), tail.map(f))

  override def foldLeft[B](init: B)(op: (B, A) => B): B = op(tail.foldLeft(init)(op), head)
}