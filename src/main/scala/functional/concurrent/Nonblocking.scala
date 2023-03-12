package org.practice
package functional.concurrent

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {

  trait Future[+A] {
    private[concurrent] def apply(cb: A => Unit)
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown() }
      latch.await()
      ref.get
    }

    def unit[A](a: A): Par[A] = es => new Future[A] {
      override def apply(cb: A => Unit): Unit = cb(a)
    }

    def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
      override def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }

    def eval[A](es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
      def call: Unit = r
    })

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A, B]](es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }

          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }
  }

}
