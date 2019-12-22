import scala.annotation.tailrec

sealed trait MyList[+A]

case object MyNil extends MyList[Nothing]

case class Cons[+A](value: A, next: MyList[A]) extends MyList[A]


object MyList {

  def apply[A](as: A*): MyList[A] =
    if (as.nonEmpty) {
      Cons(as.head, apply(as.tail: _*))
    } else MyNil

  def fill[A](a: A, n: Int): MyList[A] = {
    @tailrec
    def fillRec(acc: MyList[A], toReach: Int): MyList[A] = {
      toReach match {
        case z if z <= 0 => acc
        case _ => fillRec(Cons(a, acc), toReach - 1)
      }
    }

    fillRec(MyNil, n)
  }

  def head[A](ls: MyList[A]): A = get(ls, 0)

  @tailrec
  def get[A](ls: MyList[A], index: Int): A = {
    ls match {
      case MyNil => throw new RuntimeException("no more")
      case Cons(value, _) if (index == 0) => value
      case Cons(_, next) => get(next, index - 1)
    }
  }

  def revese[A](ls: MyList[A]): MyList[A] = {
    foldLeft[A, MyList[A]](ls, MyNil)((a, b) => Cons(b, a))
  }

  def sum(xs: MyList[Int]): Int = {
    xs match {
      case Cons(x, y) => x + sum(y)
      case MyNil => 0
    }
  }

  def size[A](ls: MyList[A]): Int = foldLeft[A, Int](ls, 0)((acc, _) => acc + 1)

  def zipWith[A, B](ls: MyList[A], xs: MyList[B]): MyList[(A, B)] = {
    val lSize = size(ls)
    val xSize = size(xs)
    val minSize = if (lSize < xSize) lSize else xSize

    @tailrec
    def createTuple(ls: MyList[A], xs: MyList[B], acc: MyList[(A, B)], index: Int, till: Int): MyList[(A, B)] = {
      if (index + 1 == till) {
        acc
      } else {
        val lse = get(ls, index)
        val xse = get(xs, index)
        createTuple(ls, xs, Cons((lse, xse), acc), index + 1, till)
      }
    }

    revese(createTuple(ls, xs, MyNil, 0, minSize))
  }


  //Excercise 3.2
  def tail[A](xs: MyList[A]): MyList[A] = {
    drop(xs, 1)
  }

  //Excercise 3.3
  def setHead[A](x: A, xs: MyList[A]): MyList[A] = {
    xs match {
      case MyNil => Cons(x, MyNil)
      case Cons(_, next) => Cons(x, next)
    }
  }

  @tailrec
  def drop[A](xs: MyList[A], n: Int): MyList[A] = {
    if (n <= 0) {
      xs
    } else {
      xs match {
        case MyNil => xs
        case Cons(_, next) => drop(next, n - 1)
      }
    }
  }

  def dropWhile[A](xs: MyList[A], shouldDrop: A => Boolean): MyList[A] = {
    xs match {
      case MyNil => xs
      case Cons(x, next) => if (shouldDrop(x)) {
        dropWhile(next, shouldDrop)
      } else {
        Cons(x, dropWhile(next, shouldDrop))
      }

    }
  }

  def init[A](xs: MyList[A]): MyList[A] = {
    xs match {
      case MyNil => xs
      case Cons(value, MyNil) => MyNil
      case Cons(value, next) => Cons(value, init(next))
    }
  }

  def foldLeft[A, B](ls: MyList[A], acc: B)(f: (B, A) => B): B = {
    ls match {
      case MyNil => acc
      case Cons(value, next) => foldLeft(next, f(acc, value))(f)
    }
  }

  def foldRight[A, B](ls: MyList[A], zero: B)(f: (A, B) => B): B = {
    ls match {
      case MyNil => zero
      case Cons(value, next) => f(value, foldRight(next, zero)(f))
    }
  }

  def length[A](ls: MyList[A]): Int = foldRight(ls, 0)((a, b) => b + 1)

  def concat[A](ls: MyList[A], ys: MyList[A]): MyList[A] = {
    (ls, ys) match {
      case (MyNil, xs) => xs
      case (zs, MyNil) => zs
      case (xs, ks) => xs match {
        case MyNil => ks
        case Cons(value, next) => Cons(value, concat(next, ks))
      }
    }
  }

  def flatMap[A, B](ls: MyList[A])(f: A => MyList[B]): MyList[B] = {
    ls match {
      case MyNil => MyNil
      case Cons(value, next) => concat(f(value), flatMap(next)(f))
    }
  }

  def mapFM[A, B](ls: MyList[A], f: A => B): MyList[B] = {
    flatMap(ls)(a => Cons(f(a), MyNil))
  }

  def map[A, B](ls: MyList[A], f: A => B): MyList[B] = {
    ls match {
      case MyNil => MyNil
      case Cons(value, next) => Cons(f(value), map(next, f))
    }
  }

  def filter[A](ls: MyList[A], f: A => Boolean): MyList[A] = {
    ls match {
      case MyNil => MyNil
      case Cons(value, next) => if (f(value)) Cons(value, filter(next, f)) else filter(next, f)
    }
  }

}