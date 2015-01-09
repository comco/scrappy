package com.github.comco.scrappy

sealed abstract class Shape

object Shape {
  type Any = Shape

  sealed abstract class Concrete extends Shape

  sealed abstract class Primitive[Raw] extends Shape

  sealed abstract class Struct extends Concrete

  sealed abstract class Tuple extends Concrete

  sealed abstract class Tuple1[+Coordinate1 <: Shape.Any] extends Tuple

  sealed abstract class Tuple2[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any] extends Tuple

  sealed abstract class Sequence[+Element <: Shape.Any] extends Concrete

  sealed abstract class Optional[+Value <: Shape.Concrete] extends Shape

  sealed abstract class Some[+Value <: Shape.Concrete] extends Optional[Value]

  sealed abstract class None extends Optional[Nothing]
}