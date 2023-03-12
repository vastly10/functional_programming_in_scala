package org.practice
package functional.concurrent

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  // 실제로 계산을 실행하여 Par로부터 값을 추출한다.
  def run[A](es: ExecutorService)(p: Par[A]): Future[A] = p(es)

  // 상수 값을 병렬 계산으로 승격한다.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  // 두 병렬 계산의 결과들을 BiFunction으로 조합한다.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  // 평가되지 않은 인수를 Par로 감싸서 병렬계산 대상임을 표시한다.
  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  // 주어진 인수가 병렬 대상임을 표시한다. run에서 평가된다.
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call(): A = a(es).get
  })

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a)) // unit(()) 의 ()은 no-op(무연산) 함수이다.
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = sequence(ps.map(asyncF(f)))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((pa, pl) => map2(pa, pl)(_ :: _))

  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  def sequenceBalance[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (ps.isEmpty) unit(Vector())
    else if (ps.length == 1) map(ps.head)(a => Vector(a))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalance(l), sequenceBalance(r))(_ ++ _)
    }
  }

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = ps map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // flatten으로 리스트를 연결할 수 있다.
  }

  def reduce[A](as: IndexedSeq[A], acc: A)(f: (A, A) => A): Par[A] = {
    if (as.size <= 1) {
      println(as)
      unit(as.headOption getOrElse acc)
    }
    else {
      println(as)
      val (l, r) = as.splitAt(as.size / 2)
      map2(fork(reduce(l, acc)(f)), fork(reduce(r, acc)(f)))(f)
    }
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond)(if (_) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n)(choices(_))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = chooser(key)(choices(_))

  def chooser[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get
      run(es)(f(a))
    }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get
      run(es)(f(a))
    }

  def flatMap2[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = join(map(pa)(f))

  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())

  def join2[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

}

object Example {

  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }

  def sum2(ints: IndexedSeq[Int]): Par[Int] = reduce(ints, 0)(_ + _)

  def max(ints: IndexedSeq[Int]): Par[Int] = reduce(ints, 0)((a, b) => if (a > b) a else b)

  def main(args: Array[String]): Unit = {
    val vec = Vector(1, 2, 3, 4, 5, 100)
    val executor = Executors.newFixedThreadPool(3)
    //    val resultSum = sum2(vec)(executor).get()
    //    val resultMax = max(vec)(executor).get()
    //    println(resultSum)
    //    println(resultMax)
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val resultFilter = parFilter(l)(a => a % 2 == 0)(executor).get
    println(resultFilter)
    executor.shutdown()
  }
}