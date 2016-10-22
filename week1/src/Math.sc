object Math {

  def abs(x: Double) = if (x < 0) -x else x

  /**
    * Acts the same as x && y.
    */
  def and(x: Boolean, y: => Boolean) = if (!x) false else y

  /**
    * Acts the same as x || y;
    */
  def or(x: Boolean, y: => Boolean) = if (x) true else y

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
    * Calculates the factorial of number n.
   */
  def factorial(n: Int) : Int =
    if (n == 0) 1 else n * factorial(n-1)

  /**
    * Tail recursive factorial.
    */
  def factorial(n: Int, acc: Int) : Int =
    if (n == 0) acc else factorial(n - 1, n * acc)

  /**
    * Greatest common divisor.
    */
  def gcd(a: Int, b: Int) : Int =
    if (b == 0) a else gcd(b, a % b)


  factorial(4)
  factorial(4, 1)
}