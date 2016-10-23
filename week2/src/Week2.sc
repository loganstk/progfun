/**
  * Week 2
  * Higher order functions, currying.
  */
def id(x: Int) = x

def cube(x: Int) = x * x * x

def factorial(x: Int): Int = if (x == 0) 1 else x * factorial(x - 1)

/**
  * Sums f(x) where x is in range (a, b).
  */
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f, a + 1, b)

def sum(f: Int => Int)(a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}

def sumInts(a: Int, b: Int) = sum(x => x,  a, b)
def sumCubes(a: Int, b: Int) = sum(x => x * x * x,  a, b)
def sumFact(a: Int, b: Int) = sum(x => factorial(x), a, b)


sum(x => x * x * x)(3, 4)
sum(cube)(3, 4)
sumCubes(3, 4)

/**
  * Function returning a function.
  */
def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  sumF
}

/**
  * Multiple parameter lists.
  */
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)
}

/**
  * No need to pass a and b each time
  * we declare a new function.
  */
def sumInts = sum(x => x) _
def sumCubes = sum(x => x * x * x) _
def sumFact = sum(x => factorial(x)) _

sumCubes(1, 10)

