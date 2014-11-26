package com.github.comco.scrappy

sealed abstract class Shape

object Shape {
  type Any = Shape
  sealed abstract class Concrete extends Any
  sealed abstract class Primitive[+RawType] extends Concrete
  sealed abstract class Struct extends Concrete
  sealed abstract class Tuple extends Concrete
  sealed abstract class Seq[+ElementShape <: Any] extends Concrete
  sealed abstract class Optional[+ValueShape <: Any] extends Any
  sealed abstract class Some[+ValueShape <: Any] extends Optional[ValueShape]
  sealed abstract class None extends Optional[Nil]
  type Nil = Nothing
}