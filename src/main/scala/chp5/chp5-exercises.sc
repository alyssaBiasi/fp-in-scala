import chp5.Stream2

Stream2.cons(1, Stream2.empty).toList

Stream2.apply(1,2,3,4,5,6).take(4).toList
Stream2.apply(1,2,3,4,5,6).drop(4).toList

Stream2.apply(2,4,6,7,8,9).takeWhile(_ % 2 == 0).toList
Stream2.apply(2,4,5,6,7).takeWhileWithFold(_ % 2 == 0).toList

Stream2.apply(2,4,6,7,8,9).exists(_ % 2 != 0)
Stream2.apply(2,4,6,8).exists(_ % 2 != 0)

Stream2.apply(2,4,6,8).forAll(_ % 2 == 0)
Stream2.apply(2,4,6,3,8).forAll(_ % 2 == 0)

Stream2.apply(2,3).headOption
Stream2.empty.headOption

Stream2.empty.map2(identity)
