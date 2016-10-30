/**
  * Week 2. Higher order functions
  */

/**
  * Sum of integers in range (a, b).
  */
def sumOfInts(a: Int, b: Int): Int =
  if (a > b) 0 else a + sumOfInts(a + 1, b)

/**
  * Sum of x pow 3 for x in range (a, b)
  */
def sumOfCubes(a: Int, b: Int): Int =
  if (a > b) 0 else a * a * a + sumOfCubes(a + 1, b)

/**
  * We can pass functions as parameters to other functions...
  */
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f, a + 1, b)

/**
  * ... as well as return function as a result.
  */
def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  sumF
}

def sumOfSquares(a: Int, b: Int) = sum(x => x * x, a, b)  // Passing anonymous function as parameter

sumOfSquares(3, 4)
sum(x => x * x, 3, 4)
