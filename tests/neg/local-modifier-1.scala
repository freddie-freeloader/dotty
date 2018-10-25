object Test {
  local val l: Int = 42

  // Simple return position

  def f1(y: Int): Int = l // error
  def f2(local y: Int): Int = y // error

  // Simple value definition

  val x1: Int = l // error
  local val x2: Int = l // OK

  // Simple assignments

  var x3: Int = 0
  x3 = l // error

  local var x4: Int = 0
  x4 = l // error

  // Simple application

  def foo_local(local y: Int) = 42
  foo_local(l) // OK

  def foo(x: Int) = x
  foo(l) // error

  // Simple Blocks

  def f3(local y: Int) = {
    y // OK
    y // error
  }

  // Simple HOF

  def f4(g: local Int => Unit)(local y: Int) = g(y) // OK
  def f5(g: Int => Unit)(local y: Int) = g(y) // error

  // Capturing second class values

  def f6(y: Int) = {
    l // error
    42
  }
  local def f7(y: Int) = {
    l // OK
    42
  }
  val f8 = { (y:Int) => l + y } // error
  local val f9 = { (y: Int) => l + y } // OK

  def map[A,B](xs: List[A])(f: A => B) = xs.map(f)
  def lmap[A,B](xs: List[A])(local f: A => B) : List[B] =
    xs match { case y::ys => f(y)::lmap(ys)(f); case Nil => Nil }

  map(List(1,2))(l + (_:Int)) // error
  lmap(List(1,2))(l + (_:Int)) // OK

  // Safe modifier

  safe val x5 = l // OK
  def escape0[A](local y: A) = y // error
  safe def escape[A](local y: A) = y // OK

  // No currying for local

  def f(local x: Int)(local g: local Int => Unit) = g(x)
  // Misc
  /*
   These are big errors
  def f10(g: local Int => Int) = g
  f10(local (x:Int) => x) // OK
  f10((x:Int) => x) // should be problem

  type T = local Int
   */
}
