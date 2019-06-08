package chp6.candymachine

import chp6.State

object CandyMachine {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val actions: List[State[Machine, Unit]] = inputs.map(i => State.modify[Machine](apply(i)))

    for {
      _ <- State.sequence(actions)
      m <- State.get[Machine]
    } yield (m.coins, m.candies)

//    val a: State[Machine, List[Unit]] = State.sequence(actions)
//
//    a.flatMap(
//      _ => State.get[Machine].map[(Int, Int)](
//        m => (m.coins, m.candies)
//      )
//    )
  }

  private def apply(input: Input)(machine: Machine): Machine = {
    (input, machine) match {
      case (_, Machine(_, 0, _)) => machine
      case (Coin, Machine(false, _, _)) => machine
      case (Coin, Machine(true, candies, coins)) => Machine(locked = false, candies, coins + 1)
      case (Turn, Machine(true, _, _)) => machine
      case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
    }
  }
}
