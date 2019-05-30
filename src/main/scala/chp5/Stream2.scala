package chp5

sealed trait Stream2[+A] {

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream2[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream2[A] = {
    this match {
      case Cons(h, t) if n > 1 => Stream2.cons(h(), t().take(n-1))
      case Cons(h, _) if n == 1 => Stream2.cons(h(), Stream2.empty)
      case _ => Stream2.empty
    }
  }

  def drop(n: Int): Stream2[A] = {
    this match {
      case Cons(_, t) if n > 1 => t().drop(n - 1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream2[A] = {
    this match {
      case Cons(h, t) if p(h()) => Stream2.cons(h(), t().takeWhile(p))
      case _ => Stream2.empty
    }
  }
}

case object Empty extends Stream2[Nothing]
case class Cons[+A](h: () => A, t: () => Stream2[A]) extends Stream2[A]

object Stream2 {
  def cons[A](hd: => A, tl: => Stream2[A]): Stream2[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream2[A] = Empty

  def apply[A](as: A*): Stream2[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }
}
