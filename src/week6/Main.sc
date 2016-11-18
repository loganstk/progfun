
  def isort(xs: List[Int]): List[Int] = {
    def insert(x: Int, xs: List[Int]): List[Int] = xs match {
      case Nil => List(x)
      case y :: ys => if (x > y) y :: insert(x, ys) else x :: xs
    }
    xs match {
      case Nil => Nil
      case y :: ys => insert(y, isort(ys))
    }
  }

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (first, second) = xs splitAt n
      merge(msort(first), msort(second))
    }
  }


  val nums = List(4, 3, 1, 22, 54, 11, 8, 32)

  isort(nums)
  msort(nums)


  def decorate(s: String = "Hello", l: String = "[", r: String = "]"): String = l + s + r

  val s = "HelloWorld"
  s exists (c => c.isUpper)

  (1 to 10) flatMap(x => (1 to 5) map (y => (x, y)))

  def dotProduct(xs: Vector[Int], ys: Vector[Int]) =
    (xs zip ys).map{case (x, y) => x * y}.sum

  dotProduct(Vector(1, 2, 3), Vector(3, 2, 1))

  def isPrime(n: Int) = (2 until n) forall (d => n % d != 0)