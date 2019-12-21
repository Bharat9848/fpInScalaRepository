import scala.annotation.tailrec

object Chap2Excercise {

 //2.1
  def fib(n: Int) = {
    @tailrec
    def fibTailRec(last: Int, secLast: Int, current: Int, toReach: Int): Int = {
      if (current == toReach) {
        last + secLast
      } else {
        fibTailRec(secLast, last + secLast, current + 1, toReach)
      }
    }
    n match {
      case i if i < 0 => i
      case 0 => 0
      case 1 => 1
      case _ => fibTailRec(0, 1, 2, n)
    }
  }

  //2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def ex2Point2IsSortedRec(index: Int, sorted: Boolean): Boolean = {
      if (index == as.length || !sorted) {
        sorted
      } else {
        ex2Point2IsSortedRec(index + 1, sorted && ordered(as(index - 1), as(index)))
      }
    }

    if (as.length <= 1) {
      true
    } else {
      ex2Point2IsSortedRec(1, true)
    }

  }

  //2.3
  def curry[A, B, C](f: (A,B) => C) : A => B => C = a => b => f(a, b)

  //2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  //2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(isSorted(Array[Int](9, 2, 3, 6, 8), (i: Int, j: Int) => i < j))
    println(curry[Int, Int, Int]((a, b)=> a + b)(3)(4))
    println(uncurry[Int, Int, Int](a => b => a + b)(3, 4))
  }

}