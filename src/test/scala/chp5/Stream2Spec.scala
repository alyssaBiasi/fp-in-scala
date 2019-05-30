package chp5

import org.specs2.mutable.Specification

class Stream2Spec extends Specification {

  "Stream" should {
    "toList" in {
      val result = Stream2.cons(1, Stream2.empty).toList
      result must containTheSameElementsAs(List(1))
    }

    "take" in {
      val result = Stream2.apply(1,2,3,4,5,6).take(4).toList
      result must containTheSameElementsAs(List(1, 2, 3, 4))
    }

    "drop" in {
      val result = Stream2.apply(1,2,3,4,5,6).drop(4).toList
      result must containTheSameElementsAs(List(4,5,6))
    }

    "takeWhile" in {
      val result = Stream2.apply(2,4,6,7,8,9).takeWhile(_ % 2 == 0).toList
      result must containTheSameElementsAs(List(2, 4, 6))
    }
  }

}
