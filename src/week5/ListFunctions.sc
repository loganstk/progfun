def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

last(List(1, 2, 3))

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => Nil
  case y :: ys => y :: init(ys)
}

init(List(1, 2, 3))

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case Nil => ys;
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs;
  case y :: ys => reverse(ys) ++ List(y)
}

reverse(List(3, 2, 1))

def removeAt[T](n: Int, xs: List[T]): List[T] =
  (xs take n) ::: (xs drop n+1)

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
      case Nil => ys
      case x :: xs1 => ys match {
        case Nil => xs
        case y :: ys1 =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    }
    val (left, right) = xs splitAt n
    merge(msort(left), msort(right))
  }
}

msort(List(1, 4, 7, 6, 5))

implicit val ordering = Ordering.Int

def qsort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  if (xs.length <= 1) return xs

  val pivot = xs(xs.length / 2)
  val (left, right) = xs.partition(e => ord.lt(e, pivot))

  def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
    case (Nil, ys1) => ys1
    case (xs1, Nil) => xs1
    case (x :: xs1, y :: ys1) =>
      if (ord.lt(x, y)) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
  }
  merge(qsort(left), qsort(right))
}

qsort(List(1, 4, 7, 6, 5))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (packed, rest) = xs span(e => e.equals(x))
    packed :: pack(rest)
  }
}

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs).map(ys => (ys.head, ys.length))
}


pack(List("a", "a", "a", "b", "c", "c", "a"))

encode(List("a", "a", "a", "b", "c", "c", "a"))


