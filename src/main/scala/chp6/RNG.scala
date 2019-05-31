package chp6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    val value = if (i < 0) -(i + 1) else i

    (value, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val value = i / (Int.MaxValue.toDouble + 1)

    (value, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (d, r2) = double(r)

    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (value, r) = intDouble(rng)

    (value.swap, r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0)
      (Nil, rng)
    else {
      val (i, r) = nonNegativeInt(rng)
      val (xs, r2) = ints(count-1)(r)

      (i :: xs, r2)
    }
  }
}
