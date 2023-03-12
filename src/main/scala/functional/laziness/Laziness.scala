package org.practice
package functional.laziness


import scala.annotation.tailrec

object Laziness {

  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond) onTrue() else onFalse()

  def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0
  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }

  def main(args: Array[String]): Unit = {

  }
}

sealed trait Stream[+A] {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail) // lazy로 캐싱하여 내려보낸다.
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOption2: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t)

  def find(p: A => Boolean): Option[A] = filter(p).headOption // 전부다 순회하지 않는다.

  def constant[A](a: A): Stream[A] = cons(a, constant(a)) // tail이 필요하기 전까지 평가되지 않는다.

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(x: Int, y: Int): Stream[Int] =
      cons(x, go(y, x + y))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty
    }

  def fibs2(): Stream[Int] = unfold((0, 1)){ case (f1, f2) => Some((f2, (f2, f1 + f2)))}

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def constant2[A](n: A) = unfold(n)(n => Some(n, n))

  def ones() = unfold(1)(_ => Some(1, 1))

  def map2[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def take2(n: Int): Stream[A] = unfold((this, n)) { // 상태를 튜플로 넘길 수 있다.
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def takeWhile3(p: A => Boolean) = unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C) = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s => Some((s, s drop 1))
    case Empty => None
  } append empty

  def main(args: Array[String]): Unit = {
    LazyList(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
    // cons(11, stream(2, 3, 4).map(_ + 10)).filter(_ % 2 == 0).toList 생성자 tail은 평가되지 않는다.
    LazyList(1, 2, 3, 4).find(_ == 3)
    // LazyList(1, 2, 3, 4).filter(_ == 3).headOption
    // LazyList(2, 3, 4).filter(_ == 3).headOption
    // LazyList(3, 4).filter(_ == 3).headOption
    // cons(3, LazyList(4).filter(_ == 3)).headOption
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


