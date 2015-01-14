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

  sealed abstract class RichOptional[+Value <: Shape.Concrete] extends Optional[Value] {
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
      schema: Schema.Primitive[Raw],
      origin: Origin.Primitive[Raw],
      raw: Raw): RichPrimitive[Raw]

    def struct(
      schema: Schema.Struct,
      origin: Origin.Struct,
      features: Map[String, Data[Shape.Any]]): RichStruct

    def tuple(
      schema: Schema.Tuple,
      origin: Origin.Tuple,
      coordinates: IndexedSeq[Data[Shape.Any]]): RichTuple

    def tuple[Coordinate1 <: Shape.Any](
      schema: Schema.Tuple1[Coordinate1],
      origin: Origin.Tuple1[Coordinate1],
      coordinate1: Data[Coordinate1]): RichTuple1[Coordinate1]

    def tuple[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
      schema: Schema.Tuple2[Coordinate1, Coordinate2],
      origin: Origin.Tuple2[Coordinate1, Coordinate2],
      coordinate1: Data[Coordinate1],
      coordinate2: Data[Coordinate2]): RichTuple2[Coordinate1, Coordinate2]

    def sequence[Element <: Shape.Any](
      schema: Schema.Sequence[Element],
      origin: Origin.Sequence[Element],
      elements: Seq[Data[Element]]): RichSequence[Element]

    def some[Value <: Shape.Concrete](
      schema: Schema.Optional[Value],
      origin: Origin.Optional[Value],
      value: Data[Value]): RichSome[Value]

    def none(origin: Origin.None): RichNone
  }

  object Primitive {
    def apply[Raw: TypeTag](
      schema: Schema.Primitive[Raw],
      origin: Origin.Primitive[Raw],
      raw: Raw)(
        implicit factory: Factory) =
      factory.primitive(schema, origin, raw)

    def apply[Raw: TypeTag](
      origin: Origin.Primitive[Raw],
      raw: Raw)(
        implicit factory: Factory): RichPrimitive[Raw] =
      apply(Schema.Primitive[Raw], origin, raw)

    def apply[Raw: TypeTag](raw: Raw)(implicit factory: Factory): RichPrimitive[Raw] =
      apply(Origin.Bare, raw)
  }

  object Struct {
    def apply(
      schema: Schema.Struct,
      origin: Origin.Struct,
      features: Map[String, Data.Any])(
        implicit factory: Factory): RichStruct =
      factory.struct(schema, origin, features)

    def apply(
      schema: Schema.Struct,
      features: Map[String, Data.Any])(
        implicit factory: Factory): RichStruct =
      apply(schema, Origin.Bare, features)
  }

  object Tuple {
    def apply(
      schema: Schema.Tuple,
      origin: Origin.Tuple,
      coordinates: IndexedSeq[Data.Any])(
        implicit factory: Factory): RichTuple =
      factory.tuple(schema, origin, coordinates)

    def apply(
      origin: Origin.Tuple,
      coordinates: IndexedSeq[Data.Any])(
        implicit factory: Factory): RichTuple =
      apply(Schema.Tuple(coordinates.map(_.schema)), origin, coordinates)

    def apply(coordinates: IndexedSeq[Data.Any])(
      implicit factory: Factory): RichTuple =
      apply(Origin.Bare, coordinates)

    def apply[Coordinate1 <: Shape.Any](
      schema: Schema.Tuple1[Coordinate1],
      origin: Origin.Tuple1[Coordinate1],
      coordinate1: Data[Coordinate1])(
        implicit factory: Factory): RichTuple1[Coordinate1] =
      factory.tuple(schema, origin, coordinate1)

    def apply[Coordinate1 <: Shape.Any](
      origin: Origin.Tuple1[Coordinate1],
      coordinate1: Data[Coordinate1])(
        implicit factory: Factory): RichTuple1[Coordinate1] =
      apply(Schema.Tuple(coordinate1.schema), origin, coordinate1)

    def apply[Coordinate1 <: Shape.Any](coordinate1: Data[Coordinate1])(
      implicit factory: Factory): RichTuple1[Coordinate1] =
      apply(Origin.Bare, coordinate1)

    def apply[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
      schema: Schema.Tuple2[Coordinate1, Coordinate2],
      origin: Origin.Tuple2[Coordinate1, Coordinate2],
      coordinate1: Data[Coordinate1],
      coordinate2: Data[Coordinate2])(
        implicit factory: Factory): RichTuple2[Coordinate1, Coordinate2] =
      factory.tuple(schema, origin, coordinate1, coordinate2)

    def apply[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
      origin: Origin.Tuple2[Coordinate1, Coordinate2],
      coordinate1: Data[Coordinate1],
      coordinate2: Data[Coordinate2])(
        implicit factory: Factory): RichTuple2[Coordinate1, Coordinate2] =
      apply(Schema.Tuple(coordinate1.schema, coordinate2.schema), origin, coordinate1, coordinate2)

    def apply[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
      coordinate1: Data[Coordinate1],
      coordinate2: Data[Coordinate2])(
        implicit factory: Factory): RichTuple2[Coordinate1, Coordinate2] =
      apply(Origin.Bare, coordinate1, coordinate2)
  }

  object Sequence {
    def apply[Element <: Shape.Any](
      schema: Schema.Sequence[Element],
      origin: Origin.Sequence[Element],
      elements: Seq[Data[Element]])(
        implicit factory: Factory): RichSequence[Element] =
      factory.sequence(schema, origin, elements)

    def apply[Element <: Shape.Any](
      origin: Origin.Sequence[Element],
      elements: Seq[Data[Element]])(
        implicit factory: Factory): RichSequence[Element] =
      apply(Schema.Sequence(elements.head.schema), origin, elements)

    def apply[Element <: Shape.Any](
      elements: Seq[Data[Element]])(
        implicit factory: Factory): RichSequence[Element] =
      apply(Origin.Bare, elements)

    def apply[Element <: Shape.Any](
      elements: Data[Element]*)(
        implicit factory: Factory, dummyImplicit: DummyImplicit): RichSequence[Element] =
      apply(elements)
  }

  object Some {
    def apply[Value <: Shape.Concrete](
      schema: Schema.Optional[Value],
      origin: Origin.Optional[Value],
      value: Data[Value])(
        implicit factory: Factory): RichSome[Value] =
      factory.some(schema, origin, value)

    def apply[Value <: Shape.Concrete](
      origin: Origin.Optional[Value],
      value: Data[Value])(
        implicit factory: Factory): RichSome[Value] =
      apply(Schema.Optional(value.schema), origin, value)

    def apply[Value <: Shape.Concrete](value: Data[Value])(
      implicit factory: Factory): RichSome[Value] =
      apply(Origin.Bare, value)
  }

  object None {
    def apply(origin: Origin.None = Origin.Bare)(implicit factory: Factory) = factory.none(origin)
  }
}

