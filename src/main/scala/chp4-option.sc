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

def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
  a match {
    case Nil => Some(Nil)
    case h ::xs => f(h).flatMap(
      y => traverse(xs)(f).map(y :: _)
    )
  }
}

def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] = {
  traverse(a)(identity)
}
