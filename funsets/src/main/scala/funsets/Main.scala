package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  var s = (x: Int) => x == 2 || x == 3
  var f = (x: Int) => x-1

  val bound = 1000
  def iter(a: Int): Unit = {
    if (contains(map(s,f), a)) println(a)
    if (a < bound) iter(a+1)
  }
  iter(-bound)

  println(forall(s, (x: Int) => x == 2 || x == 3))
}
