
def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
  es match {
    case Nil => Right(Nil)
    case h :: t => f(h).flatMap(
      y => traverse(t)(f).map(y :: _)
    )
  }
}

def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
  traverse(es)(identity)
}

