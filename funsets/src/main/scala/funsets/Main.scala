package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  union(Set(1,2,3), Set(4, 1000)).toString()


}
