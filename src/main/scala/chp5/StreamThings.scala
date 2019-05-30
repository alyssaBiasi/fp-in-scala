package chp5

object StreamThings {

  def ones: Stream2[Int] = {
    Stream2.unfold(1)(_ => Some((1,1)))
  }

  def from(n: Int): Stream2[Int] = {
    Stream2.cons(n, from(n+1))
  }

  def fromWithUnfold(n: Int): Stream2[Int] = {
    Stream2.unfold(n)(x => Some((x, x+1)))
  }

  def fibs: Stream2[Int] = {
    def f(x: Int, y: Int): Stream2[Int] = {
      Stream2.cons(x, f(y, x+y))
    }

    f(0, 1)
  }

  def fibsWithUnfold: Stream2[Int] = {
    Stream2.unfold((0, 1)) {
      case (x, y) => Some((x, (y, x + y)))
    }
  }

}
