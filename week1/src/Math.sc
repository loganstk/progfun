object Math {
  /**
    * Acts the same as x && y.
    */
  def and(x: Boolean, y: => Boolean) = if (!x) false else y

  /**
    * Acts the same as x || y;
    */
  def or(x: Boolean, y: => Boolean) = if (x) true else y

  /**
    * Return absolute value of number x.
    */
  def abs(x: Double) = if (x < 0) -x else x

  /**
    * Calculates the square root of x.
    */
  def sqrt(x: Double) = {
    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def improve(guess: Double) =
      (guess + x / guess) / 2
    sqrtIter(1.0)
  }

  /**
    * Tail recursive factorial.
   */
  def factorial(n: Int) = {
    def loop(n: Int, acc: Int) : Int =
      if (n == 0) acc else loop(n - 1, acc * n)
    loop(n, 1)
  }

  /**
    * Greatest common divisor.
    */
  def gcd(a: Int, b: Int) : Int =
    if (b == 0) a else gcd(b, a % b)
}