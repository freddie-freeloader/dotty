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

  // No currying for second-class values

  def f10a(g: local Int => Unit)(local x: Int) = g(x) // OK
  def f10b(local x: Int)(g: local Int => Unit) = g(x) // error

  def f11a
    (x1: Int)
    (local x2: Int) // error
    (local x3: Int) // error
    (local x4: Int) // OK
    = 42

  def f11b
    (x1: Int)
    (local x2: Int  // OK
    ,      x3: Int  // OK
    ,      x4: Int) // OK
    = 42

}
