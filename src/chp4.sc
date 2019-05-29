def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  a.flatMap(
    valA => b.map(
      valB => f(valA, valB)
    )
  )
}

def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  a match {
    case Nil => Some(Nil)
    case h :: xs => h.flatMap(
      hh => sequence(xs).map(hh :: _)
    )
  }
}
