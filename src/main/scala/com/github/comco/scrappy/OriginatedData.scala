package com.github.comco.scrappy

import scala.reflect.runtime.universe._

sealed trait OriginatedData[+Shape <: Shape.Any] {
  def data: Data[Shape]
  def origin: Origin[Shape]

  def datatype: Type[Shape] = data.datatype
}

object OriginatedData extends Domain {
  type Abstract[+Shape <: Shape.Any] = OriginatedData[Shape]

  abstract class RichPrimitive[+RawType: TypeTag] extends Primitive[RawType]

  abstract class RichOptional[+Value <: Shape.Concrete: TypeTag] extends Optional[Value] {
    def hasValue: Boolean
  }

  abstract class RichSome[+Value <: Shape.Concrete: TypeTag] extends RichOptional[Value] with Some[Value] {
    final def hasValue = true

    def value: OriginatedData[Value]

    def data = Data.Some(value.data)
  }

  abstract class RichNone extends RichOptional[Shape.Nil] with None {
    final def hasValue = false

    def data = Data.None
  }

  abstract class RichStruct extends Struct {
    def features: Map[String, OriginatedData.Any]
  }

  abstract class RichSequence[+Element <: Shape.Any: TypeTag] extends Sequence[Element] {
    def elements: Seq[OriginatedData[Element]]
  }

  abstract class RichTuple extends Tuple {
    def coordinates: IndexedSeq[OriginatedData.Any]
  }

  abstract class RichTuple1[+Coordinate1 <: Shape.Any: TypeTag] extends RichTuple with Tuple1[Coordinate1] {
    def coordinate1: OriginatedData[Coordinate1]

    lazy val coordinates = IndexedSeq(coordinate1)
  }

  abstract class RichTuple2[+Coordinate1 <: Shape.Any: TypeTag, +Coordinate2 <: Shape.Any: TypeTag] extends RichTuple with Tuple2[Coordinate1, Coordinate2] {
    def coordinate1: OriginatedData[Coordinate1]
    def coordinate2: OriginatedData[Coordinate2]

    lazy val coordinates = IndexedSeq(coordinate1, coordinate2)
  }
}