package org.practice
package functional.testing

import Prop._
import Gen._
import functional.state._
import functional.concurrent.Par.Par
import functional.concurrent._

import java.util.concurrent.Executors

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x => x
    }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type SuccessCount = Int // 가독성에 도움이 되는 alias
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }
  case object Proved extends Result {
    def isFalsified: Boolean = false
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map {
        case (a, i) => try {
          if (f(a)) Passed
          else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified)
      .getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casePerSize = (n + (max - 1)) / max
      val props: LazyList[Prop] = LazyList.from(0).take((n min max) - 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop {(max, _, rng) =>
        p.run(max, casePerSize, rng)
      }).toList
        .reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property")
    }

  def check(p: => Boolean): Prop = Prop {
    (_, _, _) => if (p) Passed else Falsified("()", 0)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get } // custom extractor

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g map (i => s => i)

  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf1(smallInt)) {
      ns =>
        val max = ns.max
        !ns.exists(_ > max)
    }
    run(maxProp)
    val pint = Gen.choose(0, 10) map Par.unit
    val pint2 = Gen.choose(-100, 100).listOfN(choose(0, 20)).map(l =>
      l.foldLeft(Par.unit(0))((p, i) => Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))
    val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
    run(p4)
    val p5 = Prop.forAllPar(pint)(i => equal(Par.fork(i), i)) tag "fork"
    run(p5)
    val isEven = (i: Int) => i % 2 == 0
    val takeWhileProp = Prop.forAll(Gen.listOf(choose(0, 10)))(ns => ns.takeWhile(isEven).forall(isEven))
  }
}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(b.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (n => this.listOfN(n))

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))
}

// 조금 더 간편하게 자바의 함수형 인터페이스와 같은 타입을 만들어낼 수 있다.
case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int) = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(forSize(_) map f)

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => forSize(n) flatMap (f(_).forSize(n))
    SGen(g2)
  }
}

object Gen {

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => n + start % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] = // Gen[Int] Gen[Int] => Gen[(Int, Int)]
    Gen(choose(start, stopExclusive).sample.flatMap(n => choose(start, stopExclusive).sample.map(m => (n, m))))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
//    Gen(State(RNG.double)).flatMap(d => if (d < g1Threshold) g1._1 else g2._1)
    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }
}

