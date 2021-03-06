package chp6

case class State[S, +A](run: S => (A,S)) {
  def flatMap[B](f: A => State[S, B]): State[S,B] = {
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

  def map[B](f: A => B): State[S,B] = {
    flatMap(a => State.unit(f(a)))
  }

  def map2[B,C](rb: State[S,B])(f: (A,B) => C): State[S,C] = {
    flatMap(
      a => rb.map(b => f(a,b))
    )
  }
}

object State {
  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def unit[S, A](a: A): State[S, A] = {
    State(s => (a, s))
  }

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = {
    fs.foldRight[State[S,List[A]]](unit(Nil))(
      (s, acc) => s.map2(acc)(_ :: _)
    )
  }
}
