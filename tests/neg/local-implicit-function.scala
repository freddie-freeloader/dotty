object Test {
  type using[A,E] = implicit local E => A

  case class Incrementor(i1: Int) {
    def incr(i2: Int) = i1 + i2
  }

  def incr(i: Int): Int using Incrementor = implicitly[Incrementor].incr(i)

  implicit local val inc: Incrementor = Incrementor(2)

  def calculation(): Int using Incrementor = incr(40) // OK

  // Try to store
  var incMut: Incrementor = Incrementor(42)
  def returnId(implicit ina: Incrementor): Incrementor = ina
  def escape1(): Unit using Incrementor = implicitly[Incrementor] // TODO: should error
  def escape2(): Unit using Incrementor = incMut = returnId // error


  // A basic capability

  case class ExcCap(fn: Exception => Nothing)
  def trying[A](fn: implicit local ExcCap => A): Option[A] =
    Some(fn(ExcCap(x => return None)))
  def throwing(e: Exception)(implicit local excCap: ExcCap) =
    excCap.fn(e)
  def runUnsafe[A](fn: implicit () => A) = fn()
  def runSafe[A](local fn: implicit () => A) = fn()

  // legal
  trying {
    runSafe {
      throwing(new Exception);
      print("Foo")
    }
  }

  // not legal due to usage of unsafe
  trying {
    runUnsafe {
      throwing(new Exception) // error
    }
  }
}
