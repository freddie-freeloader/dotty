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
  x4 = l // OK

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
}
