package chp6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)

      (f(a,b), r2)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = {
    map2(ra, rb)((_, _))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
//    fs match {
//      case Nil => unit(Nil)
//      case h :: t => map2(h, sequence(t))(_ :: _)
//    }
    fs.foldRight[Rand[List[A]]](unit(Nil))(
      (r, acc) => map2(r, acc)(_ :: _)
    )
  }

  // `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    val value = if (i < 0) -(i + 1) else i

    (value, r)
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i%2)
  }

  def double: Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
  }

  def int: Rand[Int] = _.nextInt

  def intDouble(rng: RNG): Rand[(Int, Double)] = {
    both(int, double)
  }

  def doubleInt(rng: RNG): Rand[(Double, Int)] = {
    both(double, int)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }
}
