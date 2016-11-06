package week4.funcobj


/**
  * Type A => B is represented like this.
  */
trait Function1[-A, +B] {
  def apply(x: A) : B
}

/**
  * Same idea.
  */
trait Function2[-A1, -A2, +B] {
  def apply(x1: A1, x2: A2) : B
}

/**
  * (x: Int) => x * x
  */
class SomeAnonFunc extends Function1[Int, Int] {
  def apply(x: Int) = x * x
}

//or  new Function1[Int, Int] { def apply(x: Int) = x * x }
//

// Methods are not functions! eta-expansion (see Lambda-calculus)
