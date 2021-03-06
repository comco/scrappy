package com.github.comco.scrappy

import scala.language.higherKinds
import scala.language.implicitConversions

trait Domain {
  type Abstract[+Shape <: Shape.Any]

  type Any = Abstract[Shape.Any]

  type Concrete = Abstract[Shape.Concrete]

  type Primitive[Raw] = Abstract[Shape.Primitive[Raw]]

  type Struct = Abstract[Shape.Struct]

  type Tuple = Abstract[Shape.Tuple]

  type Tuple1[+Coordinate1 <: Shape.Any] = Abstract[Shape.Tuple1[Coordinate1]]

  type Tuple2[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any] = Abstract[Shape.Tuple2[Coordinate1, Coordinate2]]

  type Sequence[+Element <: Shape.Any] = Abstract[Shape.Sequence[Element]]

  type Optional[+Value <: Shape.Concrete] = Abstract[Shape.Optional[Value]]
  
  type None = Optional[Shape.Nil]

  type Nil = Abstract[Shape.Nil]

  type RichPrimitive[Raw] <: Primitive[Raw]
  implicit def toRichPrimitive[Raw](a: Primitive[Raw]) = a.asInstanceOf[RichPrimitive[Raw]]

  type RichStruct <: Struct
  implicit def toRichStruct(a: Struct) = a.asInstanceOf[RichStruct]

  type RichTuple <: Tuple
  implicit def toRichTuple(a: Tuple) = a.asInstanceOf[RichTuple]

  type RichTuple1[+Coordinate1 <: Shape.Any] <: RichTuple with Tuple1[Coordinate1]
  implicit def toRichTuple1[Coordinate1 <: Shape.Any](a: Tuple1[Coordinate1]) = a.asInstanceOf[RichTuple1[Coordinate1]]

  type RichTuple2[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any] <: RichTuple with Tuple2[Coordinate1, Coordinate2]
  implicit def toRichTuple2[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](a: Tuple2[Coordinate1, Coordinate2]) = a.asInstanceOf[RichTuple2[Coordinate1, Coordinate2]]

  type RichSequence[+Element <: Shape.Any] <: Sequence[Element]
  implicit def toRichSequence[Element <: Shape.Any](a: Sequence[Element]) = a.asInstanceOf[RichSequence[Element]]

  type RichOptional[+Value <: Shape.Concrete] <: Optional[Value]
  implicit def toRichOptional[Value <: Shape.Concrete](a: Optional[Value]) = a.asInstanceOf[RichOptional[Value]]
}