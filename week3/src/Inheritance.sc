abstract class Base {
  def foo = 1
  def bar: Int
}

class Sub extends Base {
  override def foo = 2  // Fuck Java with its implicit hiding!
  /*override*/ def bar = 3
}