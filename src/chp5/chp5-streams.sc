
Stream.cons(1, Stream.empty).toList

Stream.apply(1,2,3,4,5,6).take(4).toList

Stream.apply(1,2,3,4,5,6).drop(4).toList

Stream.apply(2,4,6,7,8,9).takeWhile(_ % 2 == 0).toList