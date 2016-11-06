import week3.IntSet
import week3.Empty
import week3.NonEmpty

val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
val b: Array[IntSet] = a // Arrays are not covariant in Scala!
b(0) = Empty
val s: NonEmpty = a(0)