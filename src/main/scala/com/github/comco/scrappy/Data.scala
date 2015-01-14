package com.github.comco.scrappy

import scala.reflect.runtime.universe.TypeTag
import com.github.comco.scrappy.utils.StateEquality

sealed trait Data[+Shape <: Shape.Any] {
  def schema: Schema[Shape]

  def origin: Origin[Shape]

  def structurallyEquals(that: Data[_]): Boolean = {
    // TODO
    true
  }
}

object Data extends Domain {
  type Abstract[+Shape <: Shape.Any] = Data[Shape]

  abstract class RichPrimitive[Raw: TypeTag]
      extends Primitive[Raw] with StateEquality[RichPrimitive[Raw]] {
    def raw: Raw

    protected override def state = (schema, origin, raw)
  }

  abstract class RichStruct
      extends Struct with StateEquality[RichStruct] {
    def features: Map[String, Data[Shape.Any]]

    protected override def state = (schema, origin, features)
  }

  abstract class RichTuple
      extends Tuple with StateEquality[RichTuple] {
    def coordinates: IndexedSeq[Data[Shape.Any]]

    protected override def state = (schema, coordinates)
  }

  abstract class RichTuple1[+Coordinate1 <: Shape.Any]
      extends RichTuple with Tuple1[Coordinate1] {
    def coordinate1: Data[Coordinate1]

    override def coordinates = IndexedSeq(coordinate1)
  }

  abstract class RichTuple2[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any]
      extends RichTuple with Tuple2[Coordinate1, Coordinate2] {
    def coordinate1: Data[Coordinate1]

    def coordinate2: Data[Coordinate2]

    override def coordinates = IndexedSeq(coordinate1, coordinate2)
  }

  abstract class RichSequence[+Element <: Shape.Any]
      extends Sequence[Element] with StateEquality[RichSequence[_]] {
    def elements: Seq[Data[Element]]

    protected override def state = (schema, origin, elements)
  }

  abstract class RichOptional[+Value <: Shape.Concrete] extends Optional[Value] {
    def hasValue: Boolean
  }

  abstract class RichSome[+Value <: Shape.Concrete]
      extends RichOptional[Value] with StateEquality[RichSome[_]] {
    def value: Data[Value]

    override def hasValue = true

    protected override def state = (schema, origin, value)
  }

  abstract class RichNone
      extends RichOptional[Nothing] with StateEquality[RichNone] {
    override def hasValue = false

    protected override def state = (schema, origin)
  }

  abstract class Factory {
    def primitive[Raw: TypeTag](
      raw: Raw,
      origin: Origin[Shape.Primitive[Raw]],
      schema: Schema.Primitive[Raw]): RichPrimitive[Raw]

    def struct(
      features: Map[String, Data[Shape.Any]],
      origin: Origin[Shape.Struct],
      schema: Schema.Struct): RichStruct

    def tuple(
      coordinates: IndexedSeq[Data[Shape.Any]],
      origin: Origin.Tuple,
      schema: Schema.Tuple): RichTuple

    def tuple[Coordinate1 <: Shape.Any](
      coordinate1: Data[Coordinate1],
      origin: Origin[Shape.Tuple1[Coordinate1]],
      schema: Schema.Tuple1[Coordinate1]): RichTuple1[Coordinate1]

    def tuple[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
      coordinate1: Data[Coordinate1],
      coordinate2: Data[Coordinate2],
      origin: Origin[Shape.Tuple2[Coordinate1, Coordinate2]],
      schema: Schema.Tuple2[Coordinate1, Coordinate2]): RichTuple2[Coordinate1, Coordinate2]

    def sequence[Element <: Shape.Any](
      elements: Seq[Data[Element]],
      origin: Origin[Shape.Sequence[Element]],
      schema: Schema.Sequence[Element]): RichSequence[Element]

    def some[Value <: Shape.Concrete](
      value: Data[Value],
      origin: Origin.Optional[Value],
      schema: Schema.Optional[Value]): RichSome[Value]

    def none(origin: Origin.None): RichNone
  }

  object Primitive {
    def apply[Raw: TypeTag](
      raw: Raw,
      origin: Origin.Primitive[Raw],
      schema: Schema.Primitive[Raw])(
        implicit factory: Factory) =
      factory.primitive(raw, origin, schema)
  }

  object Struct {
    def apply(
      features: Map[String, Data.Any],
      origin: Origin.Struct,
      schema: Schema.Struct)(
        implicit factory: Factory) =
      factory.struct(features, origin, schema)
  }

  object Tuple {
    def apply(
      coordinates: IndexedSeq[Data.Any],
      origin: Origin.Tuple,
      schema: Schema.Tuple)(
        implicit factory: Factory) =
      factory.tuple(coordinates, origin, schema)

    def apply[Coordinate1 <: Shape.Any](
      coordinate1: Data[Coordinate1],
      origin: Origin.Tuple1[Coordinate1],
      schema: Schema.Tuple1[Coordinate1])(
        implicit factory: Factory) =
      factory.tuple(coordinate1, origin, schema)

    def apply[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
      coordinate1: Data[Coordinate1],
      coordinate2: Data[Coordinate2],
      origin: Origin.Tuple2[Coordinate1, Coordinate2],
      schema: Schema.Tuple2[Coordinate1, Coordinate2])(
        implicit factory: Factory) =
      factory.tuple(coordinate1, coordinate2, origin, schema)
  }

  object Sequence {
    def apply[Element <: Shape.Any](
      elements: Seq[Data[Element]],
      origin: Origin.Sequence[Element],
      schema: Schema.Sequence[Element])(
        implicit factory: Factory) =
      factory.sequence(elements, origin, schema)
  }

  object Some {
    def apply[Value <: Shape.Concrete](
      value: Data[Value],
      origin: Origin.Optional[Value],
      schema: Schema.Optional[Value])(
        implicit factory: Factory) =
      factory.some(value, origin, schema)
  }

  object None {
    def apply(origin: Origin.None)(implicit factory: Factory) = factory.none(origin)
  }
}

