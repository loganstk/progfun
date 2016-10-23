import math.abs

val tolerance = 0.001

def isCloseEnough(x: Double, y: Double) =
  abs((x - y )/ x) / x  < tolerance

/**
  * x is a fixed point of function f(x)
  * if f(x) == x
  */
def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

/**
  * Stabilizing by averaging
 */
def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt(x: Double) =
  fixedPoint(averageDamp(y => x / y))(1)

sqrt(2)