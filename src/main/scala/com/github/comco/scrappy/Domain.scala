package com.github.comco.scrappy

import scala.reflect.runtime.universe.TypeTag

import scala.language.implicitConversions
import scala.language.higherKinds

/**
 * Defines the relationships between Scrappy concepts.
 */
trait Domain {
  type Abstract[+Shape <: Shape.Any]
  type Any = Abstract[Shape.Any]
  type Primitive[+RawType] = Abstract[Shape.Primitive[RawType]]
  type Struct = Abstract[Shape.Struct]
  type Tuple = Abstract[Shape.Tuple]
  type Tuple1[+Coordinate1 <: Shape.Any] = Abstract[Shape.Tuple1[Coordinate1]]
  type Tuple2[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any] = Abstract[Shape.Tuple2[Coordinate1, Coordinate2]]
  type Sequence[+Element <: Shape.Any] = Abstract[Shape.Sequence[Element]]
  type Optional[+Value <: Shape.Concrete] = Abstract[Shape.Optional[Value]]
  type Some[+Value <: Shape.Concrete] = Abstract[Shape.Some[Value]]
  type None = Abstract[Shape.None]
  type Nil = Abstract[Shape.Nil]

  type RichPrimitive[+RawType] <: Primitive[RawType]
  type RichStruct <: Struct
  type RichTuple <: Tuple
  type RichTuple1[+Coordinate1 <: Shape.Any] <: RichTuple with Tuple1[Coordinate1]
  type RichTuple2[+Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any] <: RichTuple with Tuple2[Coordinate1, Coordinate2]
  type RichSequence[+Element <: Shape.Any] <: Sequence[Element]
  type RichOptional[+Value <: Shape.Concrete] <: Optional[Value]
  type RichSome[+Value <: Shape.Concrete] <: RichOptional[Value] with Some[Value]
  type RichNone <: RichOptional[Shape.Nil] with None

  implicit def toPrimitive[RawType: TypeTag](d: Primitive[RawType]) = d.asInstanceOf[Primitive[RawType]]
  implicit def toRichStruct(d: Struct) = d.asInstanceOf[RichStruct]
  implicit def toRichTuple(d: Tuple) = d.asInstanceOf[RichTuple]
  implicit def toRichTuple1[Coordinate1 <: Shape.Any: TypeTag](d: Tuple1[Coordinate1]) =
    d.asInstanceOf[RichTuple1[Coordinate1]]
  implicit def toRichTuple2[Coordinate1 <: Shape.Any: TypeTag, Coordinate2 <: Shape.Any: TypeTag](d: Tuple2[Coordinate1, Coordinate2]) =
    d.asInstanceOf[RichTuple2[Coordinate1, Coordinate2]]
  implicit def toRichSequence[Element <: Shape.Any: TypeTag](d: Sequence[Element]) = d.asInstanceOf[RichSequence[Element]]
  implicit def toRichOptional[Value <: Shape.Concrete: TypeTag](d: Optional[Value]) = d.asInstanceOf[RichOptional[Value]]
  implicit def toRichSome[Value <: Shape.Concrete: TypeTag](d: Some[Value]) = d.asInstanceOf[RichSome[Value]]
  implicit def toRichNone(d: None) = d.asInstanceOf[RichNone]
}