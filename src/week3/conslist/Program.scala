package week3.conslist

/**
  * Created by Andrew on 30-Oct-16.
  */
object Program extends App {
  def nth[T](n: Int, xs: List[T]): T =
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth(n - 1, xs.tail)
  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

  println(nth(-1, list))
}
