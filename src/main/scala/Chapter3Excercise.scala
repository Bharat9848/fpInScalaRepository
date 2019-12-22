import MyList._

import scala.annotation.tailrec

object Chapter3Excercise {


  def main(args: Array[String]): Unit = {

    val ls = MyList(1, 2, 3, 4, 5)
    val emptyList = MyNil
    //3.1 what will be the output
    val e = ls match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case MyNil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // answer should be this
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(e)
    /*
 3.2 Implement the function tail for removing the first element of a List. Note that the function takes constant time. What are different choices you could make in your implementation if the List is Nil? We’ll return to this question in the next chapter.
  */
    println(tail(ls))
    println(tail(emptyList))
    /*
    3.3 Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
     */
    println(setHead(10, ls))
    println(setHead(10, emptyList))



    /*
    3.4 Generalize tail to the function drop, which removes the first n elements from a list. Note that this function takes time proportional only to the number of elements being dropped—we don’t need to make a copy of the entire List.
     */
    println(drop(ls, 3))
    println(drop(ls, 8))

    /*
    3.5 Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
     */
    println(dropWhile(ls, (a: Int) => a % 2 == 0))
    println(dropWhile(ls, (a: Int) => a < 10))
    /*
    3.6 Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3). Why can’t this function be implemented in constant time like tail?
     */
    println(init(ls))
    println(init(Cons(31, MyNil)))
    println(init(emptyList))
    /*
    3.7 Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list. This is a deeper question that we’ll return to in chapter 5.
     */
    println(foldRight(MyList(1.0, 2.0, 3.0), 1.0)((a, b) => a * b))
    println(foldRight(MyList[Double](), 1.0)((a, b) => a * b))
    println(foldRight(MyList(1.0, 2.0, 0.0), 1.0)((a, b) => a * b))
    /*
    3.8 See what happens when you pass Nil and Cons themselves to foldRight, like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).[10] What do you think this says about the relationship between foldRight and the data constructors of List?
     */
    println(foldRight(MyList(1.0, 2.0, 3.0), MyNil: MyList[Double])((a, b) => Cons(a, b)))
    /*
    3.9 Compute the length of a list using foldRight.
     */
    println(length(ls))
    println(length(emptyList))
    /*
     3.10 Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists (we say it’s not stack-safe). Convince yourself that this is the case, and then write another general list-recursion function, foldLeft, that is tail-recursive, using the techniques we discussed in the previous chapter. Here is its signature:[11]
     */
    val largeList = fill[Int](1, 1000000)
    //    println(foldRight(largeList,0 )(_ + _))
    println(foldLeft(largeList, 0)(_ + _))

    /*
     3.11 Write sum, product, and a function to compute the length of a list using foldLeft.
     */
    println(foldLeft(ls, 1)(_ * _))
    println(foldLeft(ls, 0)(_ + _))
    println(foldLeft(ls, 0)((a, _) => a + 1))

    /*
     3.12 Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)). See if you can write it using a fold.
     */
    println(foldRight[Int, MyList[Int]](ls, MyNil)((a, b) => {
      val last = Cons(a, MyNil)
      concat(b, last)
    }))

    println(foldLeft[Int, MyList[Int]](ls, MyNil)((b, a) => Cons(a, b)))

    /*
    3.13 Hard: Can you write foldLeft in terms of foldRight? How about the other way around? Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively, which means it works even for large lists without overflowing the stack.
     */
    /*
    3.14 Implement append in terms of either foldLeft or foldRight.
     */
    val xs = MyList(1, 2, 3, 4, 5)
    val ys = MyList(6, 7, 8, 9, 10)
    println(foldLeft(ys, xs)((b, a) => concat(b, Cons(a, MyNil))))
    println(foldRight(xs, ys)((a, b) => Cons(a, b)))

    /*
     3.15 Hard: Write a function that concatenates a list of lists into a single list. Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.
     */
    val xss = MyList(xs, emptyList, ys, ls)
    println(foldRight[MyList[Int], MyList[Int]](xss, MyNil)((a, b) => concat(a, b)))
    println(foldLeft[MyList[Int], MyList[Int]](xss, MyNil)((a, b) => concat(a, b)))
    /*
    3.16 Write a function that transforms a list of integers by adding 1 to each element. (Reminder: this should be a pure function that returns a new List!)
     */
    println(foldLeft[Int, MyList[Int]](ys, MyNil)((acc, b) => concat(acc, Cons(b + 1, MyNil))))
    println(foldRight[Int, MyList[Int]](ys, MyNil)((a, b) => Cons(a + 1, b)))
    println(map[Int, Int](ls, _ + 1))
    /*
    3.17 3.18 Write a function that turns each value in a List[Double] into a String. You can use the expression d.toString to convert some d: Double to a String.
     */
    val ds = MyList(1.0, 2.0, 4.0, 6.0)
    println(map[Double, String](ds, _.toString))

    /*
    3.19 Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remove all odd numbers from a List[Int].
     */
    println(filter[Int](ls, _ % 2 == 0))
    /*
    3.20 Write a function flatMap that works like map except that the function given will return a list instead of a single result, and that list should be inserted into the final resulting list. Here is its signature:
     */
    println(flatMap(ls)(a => Cons(a, Cons(a, MyNil))))
    /*
    3.21 Use flatMap to implement filter.
     */
    println(flatMap(ls)(a => {
      val f = (k:Int) =>  (k % 2 == 0)
      if (f(a)) MyList(a) else MyNil}))
  /*
  3.22 Write a function that accepts two lists and constructs a new list by adding corresponding elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
   */
    val sumOneToOne = (ls: MyList[Int], xs: MyList[Int]) => map(zipWith(ls, xs), (a:(Int, Int)) => a._1 + a._2)

    println(sumOneToOne(xs, xs))

  /*
  3.23 Generalize the function you just wrote so that it’s not specific to integers or addition. Name your generalized function zipWith.
   */
  println(zipWith(ls, xs))
  }
}

