package week3

abstract class Base {
  val foo = 1
  def bar: Int
}

class Sub extends Base {
  override val foo = 2  // Fuck Java with its implicit hiding!
  /*override*/ def bar = 3
}