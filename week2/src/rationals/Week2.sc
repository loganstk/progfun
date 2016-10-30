class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be non-zero!")

  def numer = x
  def denom = y

  def this(x: Int) = this(x, 1)

  private def abs(x: Int) = if (x < 0) -x else x

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  private val g = abs(gcd(x, y))

  def unary_-  = new Rational(-numer, denom)

  def + (that: Rational) =
  new Rational(this.numer * that.denom + that.numer * this.denom,
  this.denom * that.denom)

  def - (that: Rational) = this + -that

  def * (that: Rational) =
    new Rational(this.numer * that.numer, this.denom * that.denom)

  def < (that: Rational) = this.numer * that.denom < that.numer / this.denom

  def > (that: Rational) = !(this < that)

  override def toString = (numer / g) + "/" + (denom / g)
}

val x = new Rational(1, 2)
val y = new Rational(1, 4)
val z = new Rational(3)

x.*(y)

x * y