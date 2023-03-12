package org.practice
package functional.datastructure

import org.practice.functional.datastructure

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match { // pattern matching
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    as match {
      case Nil => acc
      case Cons(h, t) => f(h, foldRight(t, acc)(f))
    }

  def sum2(ints: List[Int]) = foldRight(ints, 0)((x, y) => x + y)
  def product2(doubles: List[Double]) = foldRight(doubles, 0.0)(_ * _) // << (x, y) => x * y를 간단히 표기

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]) = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](head: A, l: List[A]) = l match {
    case Nil => Cons(head, Nil)
    case Cons(_, xs) => Cons(head, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f) // Pattern guard
      case _ => l // default value
    }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]) = foldRight(l, 0)((_, y) => y + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], acc: B)(f: (A, B) => B): B =
    l match {
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(x, acc))(f)
    }

  def foldRight2[A, B](l: List[A], acc: B)(f: (A, B) => B): B =
    l match {
      case Nil => acc
      case Cons(x, xs) => foldRight2(xs, foldLeft(xs, acc)(f))(f) // TODO
    }

  def append2[A](a1: List[A], a2: List[A]) = foldRight(a1, a2)(Cons(_, _))

  def flatList[A](ll: List[List[A]]) = foldRight(ll, Nil: List[A])(append2)

  def sum3(ints: List[Int]) = foldLeft(ints, 0)(_ + _)
  def product3(doubles: List[Double]) = foldLeft(doubles, 0.0)(_ * _)

  def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])(Cons(_, _))

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, y) => Cons(x + 1, y))
  def add2(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 2, add2(t))
    }

  def toStringDbList(l: List[Double]) = foldRight(l, Nil:List[String])((x, y) => Cons(x.toString, y))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, y) => Cons(f(x), y))

  // stack-safe function
  def map_2[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new ListBuffer[B]
    @tailrec
    def go(l: List[A]): Unit  = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t) // ??
    }
    go(l)
    List(buf.toList: _*) // spread operator?
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    val buf = new ListBuffer[A]
    @tailrec
    def go(as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(h, t) => if(f(h)) buf += h; go(t)
    }
    go(as)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

  def filter3[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(i => if (f(i)) List(i) else Nil)

  def addPairWise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
  }

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  def startsWith[A](a: List[A], prefix: List[A]): Boolean =
    (a, prefix) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _ => false
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }

  def main(args: Array[String]): Unit = {
    val x = List(1, 2) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => sum(t)
      case _ => 101
    }
    println(x)
    val y = List(1, 2, 3, 4)
    println(init(y))
    val l = List(1, 2, 3)
    dropWhile(l, (i: Int) => i < 4) // 번거로운 형식추론
    dropWhile2(l)(i => i < 4) // 다음을 통해 형식추론

    println(product2(List(1.0, 2.0, 0.0, 4.0)))
    println(length(List(1, 2, 3, 4)))
    println(reverse(List(1, 2, 3)))
    println(append2(List(1, 2, 3), List(4, 5)))
    println(append2(Nil, List(1, 2, 3)))
    println(flatList(List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))))
    println(add1(List(1, 2, 3)))
    println(filter3(List(1, 2, 3, 4, 5))(i => i % 2 == 1))
    println(flatMap(List(1, 2, 3))(i => List(i, i)))
    println(Cons(1, Nil) == Cons(2, Nil))
  }
}

