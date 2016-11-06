val fruits = List("pineapple", "apple")

val nums1 = 1 :: 2 :: 3 :: 4 :: Nil
//:: - prepend()
val nums2 = Nil.::(4).::(3).::(2).::(1)

// List patterns:
// 1 :: 2 :: xs
// x :: Nil
// List(x)
// List()
// List(2, xs)

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}