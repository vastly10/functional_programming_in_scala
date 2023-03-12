package org.practice
package functional.exception

import scala.util.Try

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(g) => Some(f(g))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(g) => g
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None) //

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = //
    a flatMap(aa => b map (bb => f(aa, bb)))

  def map2_for_comprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = //
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = //
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = //
    traverse_1(a)(a => a)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a map (aa => f(aa)))

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = //
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def traverse_2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = //
    a match {
      case Nil => None
      case h :: t => map2(f(h), traverse_2(t)(f))(_ :: _)
    }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(r) => Right(f(r))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(r) => f(r)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(r) => Right(r)
      case Left(_) => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (aa => b map (bb => f(aa, bb)))

  def map2_for[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(v => v)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)
object Exception {

  def failingFn(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new IllegalArgumentException("fail")): Int)
    }
    catch { case e: IllegalArgumentException => 43}
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def mean_1(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {case e: Exception => Left(e)}

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {case e : Exception => Left(e)}

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range")
    else (Right(new Age(age)))

  def makePerson(name: String, age: Int) =
    mkName(name).map2(mkAge(age))(Person)
}