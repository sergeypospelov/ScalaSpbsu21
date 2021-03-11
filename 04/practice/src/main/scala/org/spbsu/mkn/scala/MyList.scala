package org.spbsu.mkn.scala

// extraTask
trait Foldable[+A] {
  def foldLeft[B](init: B)(op: (B, A) => B): B
}

sealed trait MyList[+A] extends Foldable[A] {
  def head: A

  def tail: MyList[A]

  def drop(n: Int): MyList[A]

  def take(n: Int): MyList[A]

  def map[B](f: A => B): MyList[B]

  def ::[B >: A](elem: B): MyList[B] = Cons(elem, this)
}

object MyList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[A](seq: Seq[A]): MyList[A] = seq.foldRight[MyList[A]](Nil) { (x, xs) => Cons(x, xs) }

  def sum(intList: MyList[Int]): Int = intList match {
    case Nil => undef
    case lst => sumWithFoldLeft(lst)
  }

  def size[A](intList: MyList[A]): Int = intList.foldLeft(0)((sz, _) => sz + 1)

  // extra task: implement sum using foldLeft

  def sumWithFoldLeft(intList: MyList[Int]): Int = intList.foldLeft(0)((x, sum) => x + sum)

  // stable
  def sort[T](list: MyList[T])(implicit comparator: Ordering[T]): MyList[T] = {
    val seq = list.foldLeft(Seq[T]()) { (s, el) => s :+ el } .reverse
    MyList.fromSeq(seq.sorted)
  }

  implicit def listComparator[T](implicit innerComparator: Ordering[T]): Ordering[MyList[T]] =
    (o1: MyList[T], o2: MyList[T]) => (o1, o2) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => 1
      case (Cons(x, xs), Cons(y, ys)) => {
        val res = innerComparator.compare(x, y)
        res match {
          case 0 => listComparator.compare(xs, ys)
          case x => x
        }
      }
    }
}

case object Nil extends MyList[Nothing] {
  override def head: Nothing = MyList.undef

  override def tail: MyList[Nothing] = MyList.undef

  override def drop(n: Int): MyList[Nothing] = if (n == 0) {
    Nil
  } else {
    MyList.undef
  }

  override def take(n: Int): MyList[Nothing] = if (n == 0) {
    Nil
  } else {
    MyList.undef
  }

  override def map[B](f: Nothing => B): MyList[Nothing] = Nil

  override def foldLeft[B](init: B)(op: (B, Nothing) => B): B = init
}

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A] {

  override def drop(n: Int): MyList[A] = if (n < 0) {
    MyList.undef
  } else if (n == 0) {
    Cons(head, tail)
  } else {
    tail.drop(n - 1)
  }

  override def take(n: Int): MyList[A] = if (n < 0) {
    MyList.undef
  } else if (n == 0) {
    Nil
  }
  else {
    Cons(head, tail.take(n - 1))
  }

  override def map[B](f: A => B): MyList[B] = Cons(f(head), tail.map(f))

  override def foldLeft[B](init: B)(op: (B, A) => B): B = op(tail.foldLeft(init)(op), head)
}