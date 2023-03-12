package org.practice
package programming

import org.practice.programming.Result.Location

import scala.collection.immutable.List
import scala.util.Random

//Map
//Rule
// 룰을 조합하는 것이다.
// 각 단계는 이전 상태에 의존한다.

// Result 연산에 대한 메서드 ex) next,
// List[(Int, List[A])] 나중에 원상복귀를 위해
case class Result[A](list: List[Location[A]]) {
  def size: Int = list.size
}

object Result {
  type Location[A] = (Int, List[A])

  def createEmpty[A](size: Int): Result[A] = Result(List.fill(size)((0, List())).zipWithIndex.map(t => (t._2, t._1._2)))

  def opsToResult[A](op: ResultOps[A]): Result[A] = op.result

  def combination[A](size: Int, items: List[A])(f: (Result[A], A) => Boolean): Result[A] = ???

  case class ResultOps[A](result: Result[A], current: Int)
}

object Combinator2 {

}
