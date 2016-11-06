package week4.primobj


// Peano numbers
abstract class Natural {
  def isZero: scala.Boolean

  def predecessor: Natural

  def successor: Natural = new Successor(this)

  def +(that: Natural): Natural

  def -(that: Natural): Natural
}
1
object Zero extends Natural {
  override def isZero: scala.Boolean = true

  override def predecessor: Natural =
    throw new NoSuchElementException("Zero can't have a predecessor!")

  override def +(that: Natural): Natural = that

  override def -(that: Natural): Natural =
    if (that.isZero) this
    else throw new IllegalArgumentException("Difference is negative!")
}


class Successor(n: Natural) extends Natural {
  override def isZero: scala.Boolean = false

  override def predecessor: Natural = n

  override def +(that: Natural): Natural = new Successor(n + that)

  override def -(that: Natural): Natural =
    if (that.isZero) this
    else n - that.predecessor
}