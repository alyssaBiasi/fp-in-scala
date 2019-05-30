package chp5

import org.specs2.mutable.Specification

class Stream2Spec extends Specification {

  "Stream" should {
    "toList" in {
      val result = Stream2.cons(1, Stream2.empty).toList
      result must containTheSameElementsAs(List(1))
    }

    "take" should {
      "should return elements" in {
        val result = Stream2.apply(1,2,3,4,5,6).take(4).toList
        result must containTheSameElementsAs(List(1, 2, 3, 4))
      }

      "with unfold" should {
        "should return elements" in {
          val result = Stream2.apply(1,2,3,4,5,6).takeWithUnfold(4).toList
          result must containTheSameElementsAs(List(1, 2, 3, 4))
        }
      }
    }

    "drop" in {
      val result = Stream2.apply(1,2,3,4,5,6).drop(4).toList
      result must containTheSameElementsAs(List(4,5,6))
    }

    "takeWhile" should {
      "should return elements" in {
        val result = Stream2.apply(2, 4, 6, 7, 8, 9).takeWhile(_ % 2 == 0).toList
        result must containTheSameElementsAs(List(2, 4, 6))
      }

      "with unfold" should {
        "should return elements" in {
          val result = Stream2.apply(2, 4, 6, 7, 8, 9).takeWhileWithUnfold(_ % 2 == 0).toList
          result must containTheSameElementsAs(List(2, 4, 6))
        }
      }
    }

    "takeWhileWithFold" in {
      val result = Stream2.apply(2,4,5,6,7).takeWhileWithFold(_ % 2 == 0).toList
      result must containTheSameElementsAs(List(2, 4))
    }

    "exists" should {
      "return true" in {
        Stream2.apply(2,4,6,7,8,9).exists(_ % 2 != 0) must beTrue
      }

      "return false" in {
        Stream2.apply(2,4,6,8).exists(_ % 2 != 0) must beFalse
      }
    }

    "forAll" should {
      "return true" in {
        Stream2.apply(2,4,6,8).forAll(_ % 2 == 0) must beTrue
      }

      "return false" in {
        Stream2.apply(2,4,6,3,8).forAll(_ % 2 == 0) must beFalse
      }
    }

    "headOption" should {
      "return some" in {
        Stream2.apply(2,3).headOption must beSome(2)
      }

      "return none" in {
        Stream2.empty.headOption must beNone
      }
    }

    "map" should {
      "return transformed stream" in {
        val result = Stream2.apply(1,2,3).map(_ + 1)
        result.toList must containTheSameElementsAs(List(2,3,4))
      }

      "with unfold" should {
        "return transformed stream" in {
          val result = Stream2.apply(1,2,3).mapWithUnfold(_ + 1)
          result.toList must containTheSameElementsAs(List(2,3,4))
        }
      }
    }

    "filter" should {
      "return filtered stream" in {
        val result = Stream2.apply(1,2,3).filter(_ % 2 == 0)
        result.toList must containTheSameElementsAs(List(2))
      }
    }

    "append" should {
      "return the combined streams" in {
        val result = Stream2.apply(1,2).append(Stream2.apply(3,4))
        result.toList must containTheSameElementsAs(List(1,2,3,4))
      }
    }

    "flatMap" should {
      "return transformed stream" in {
        val result = Stream2.apply(1,2).flatMap(x => Stream2.apply(x, x + 1))
        result.toList must containTheSameElementsAs(List(1,2,2,3))
      }
    }

    "unfold" should {
      "generate a stream" in {
        val stream = Stream2.unfold(0)(x => Some((x, x+1)))

        val result = stream.take(5)
        result.toList must containTheSameElementsAs(List(0,1,2,3,4))
      }
    }

    "zipAll" should {
      "return combined streams" in {
        val stream1 = Stream2.apply(1,2,3)
        val result = stream1.zipAll(stream1)

        val expected = List(
          (Some(1), Some(1)),
          (Some(2), Some(2)),
          (Some(3), Some(3))
        )
        result.toList must containTheSameElementsAs(expected)
      }

      "return combined streams of different length" in {
        val stream1 = Stream2.apply(1,2,3)
        val result = stream1.zipAll(stream1.take(1))

        val expected = List(
          (Some(1), Some(1)),
          (Some(2), None),
          (Some(3), None)
        )
        result.toList must containTheSameElementsAs(expected)
      }
    }

  }

}
