package com.github.comco.scrappy

import scala.reflect.runtime.universe.TypeTag

sealed abstract class Shape

object Shape {
  type Any = Shape
  sealed abstract class Concrete extends Any
  sealed abstract class Primitive[+RawType: TypeTag] extends Concrete
  sealed abstract class Struct extends Concrete
  sealed abstract class Tuple extends Concrete
  sealed abstract class Tuple1[+Coordinate1 <: Any: TypeTag] extends Tuple
  sealed abstract class Tuple2[+Coordinate1 <: Any: TypeTag, +Coordinate2 <: Shape: TypeTag] extends Tuple
  sealed abstract class Sequence[+Element <: Any: TypeTag] extends Concrete
  sealed abstract class Optional[+Value <: Concrete: TypeTag] extends Any
  sealed abstract class Some[+Value <: Concrete: TypeTag] extends Optional[Value]
  sealed abstract class None extends Optional[Nothing]
  type Nil = Nothing
}