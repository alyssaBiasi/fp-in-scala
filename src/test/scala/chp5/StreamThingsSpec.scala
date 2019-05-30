package chp5

import org.specs2.mutable.Specification

class StreamThingsSpec extends Specification {

  "Stream methods" should {
    "from" should {
      "generate an incremental stream" in {
        val result = StreamThings.from(1).take(5)
        result.toList must containTheSameElementsAs(List(1,2,3,4,5))
      }

      "with unfold" should {
        "generate an incremental stream" in {
          val result = StreamThings.fromWithUnfold(1).take(5)
          result.toList must containTheSameElementsAs(List(1,2,3,4,5))
        }
      }
    }

    "fibs" should {
      "generate the fibonacci numbers" in {
        val result = StreamThings.fibs.take(7)
        result.toList must containTheSameElementsAs(List(0,1,1,2,3,5,8))
      }

      "with unfold" should {
        "generate the fibonacci numbers" in {
          val result = StreamThings.fibsWithUnfold.take(7)
          result.toList must containTheSameElementsAs(List(0,1,1,2,3,5,8))
        }
      }
    }
  }
}
