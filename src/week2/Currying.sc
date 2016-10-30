/**
  * Week 2. Currying
  */

/**
  * Multiple parameter lists.
  */
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sum(f)(a + 1, b)
}

def cube(x: Int) = x * x * x

/**
  * No need to pass a and b each time
  * we declare a new function.
  */
def sumOfCubes = sum(cube) _  // Partially applied function

// Sum of cubes:
sum(cube)(1, 3)
sum(x => x * x * x)(1, 3)
sumOfCubes(1, 3)


/**
  * Calculates a product of integers
  * in range (a, b).
  */
def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

/**
  * Defining a factorial in terms of product.
  */
def factorial(n: Int) = product(x => x)(1, n)

/**
  * MapReduce implementation.
  */
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

/**
  * Product in terms of MapReduce model
  */
def mrproduct(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x, y) => x * y, 1)(a, b)

mrproduct(x => x)(1, 5)
