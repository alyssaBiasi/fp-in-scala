package chp6.candymachine

import chp6.State
import org.specs2.mutable.Specification

class CandyMachineSpec extends Specification {

  "CandyMachine" should {
    "when running a simulation" in {
      "return the result" in {
        val simulation = CandyMachine.simulateMachine(List(Coin, Turn, Coin, Turn))
        val (result, _) = simulation.run(Machine(locked = true, candies = 10, coins = 0))

        result must beEqualTo((2, 8))
      }
    }

    "inserting a coin" should {
      val action: State[Machine, (Int, Int)] = CandyMachine.simulateMachine(List(Coin))

      "for a locked machine" in {
        "unlock it" in{
          val machine = Machine(locked = true, candies = 10, coins = 0)
          val (_, result) = action.run(machine)

          val expected = Machine(locked = false, candies = 10, coins = 1)
          result must beEqualTo(expected)
        }

        "with no candy" in {
          "be ignored" in{
            val machine = Machine(locked = true, candies = 0, coins = 0)
            val (_, result) = action.run(machine)

            result must beEqualTo(machine)
          }
        }
      }

      "for an unlocked machine" in {
        "be ignored" in {
          val machine = Machine(locked = false, candies = 10, coins = 0)
          val (_, result) = action.run(machine)

          result must beEqualTo(machine)
        }
      }
    }

    "turning the knob" should {
      val action: State[Machine, (Int, Int)] = CandyMachine.simulateMachine(List(Turn))

      "for an unlocked machine" in {
        val machine = Machine(locked = false, candies = 10, coins = 0)
        val (_, result) = action.run(machine)

        "result in candy" in {
          result.candies must beEqualTo(9)
        }

        "lock the  machine" in {
          result.locked must beTrue
        }

        "with no candy" in {
          "be ignored" in{
            val machine = Machine(locked = false, candies = 0, coins = 0)
            val (_, result) = action.run(machine)

            result must beEqualTo(machine)
          }
        }
      }

      "for an locked machine" in {
        "be ignored" in {
          val machine = Machine(locked = true, candies = 10, coins = 0)
          val (_, result) = action.run(machine)

          result must beEqualTo(machine)
        }
      }
    }
  }

}
