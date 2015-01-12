package com.github.comco.scrappy

import scala.reflect.runtime.universe.TypeTag
import com.github.comco.scrappy.utils.StateEquality

sealed trait Data[-Source <: Shape.Any, +Target <: Shape.Any] {
  def sourceSchema: Schema.Any

  def targetSchema: Schema[Shape]

  def origin: Origin[Source, Target]
}

object Data {
  type Primitive[-Source <: Shape.Any, Raw] = Data[Source, Raw]

  type Struct[-Source <: Shape.Any] = Data[Source, Shape.Struct]

  type Tuple[-Source <: Shape.Any] = Data[Source, Shape.Tuple]

  type Tuple1[-Source <: Shape.Any, +Coordinate1 <: Shape.Any] = Data[Source, Shape.Tuple1[Coordinate1]]

  type Tuple2[-Source <: Shape.Any, +Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any] = Data[Source, Shape.Tuple2[Coordinate1, Coordinate2]]

  type Sequence[-Source <: Shape.Any, +Element <: Shape.Any] = Data[Source, Shape.Sequence[Element]]

  type Optional[-Source <: Shape.Any, +Value <: Shape.Concrete] = Data[Source, Shape.Optional[Value]]

  type Some[-Source <: Shape.Any, +Value <: Shape.Concrete] = Data[Source, Shape.Some[Value]]

  type None[-Source <: Shape.Any] = Data[Source, Shape.None]

  abstract class RichPrimitive[-Source <: Shape.Any, Raw]
      extends Primitive[Source, Raw] with StateEquality[RichPrimitive[Source, Raw]] {
    def raw: Raw

    protected override def state = (sourceSchema, targetSchema, origin, raw)
  }

  abstract class RichStruct[-Source <: Shape.Any]
      extends Struct[Source] with StateEquality[RichStruct[Source]] {
    def features: Map[String, Data[Source, Shape.Any]]

    protected override def state = (sourceSchema, targetSchema, origin, features)
  }

  abstract class RichTuple[-Source <: Shape.Any]
      extends Tuple[Source] with StateEquality[RichTuple[Source]] {
    def coordinates: IndexedSeq[Data[Source, Shape.Any]]

    protected override def state = (sourceSchema, targetSchema, coordinates)
  }

  abstract class RichTuple1[-Source <: Shape.Any, +Coordinate1 <: Shape.Any]
      extends RichTuple[Source] with Tuple1[Source, Coordinate1] {
    def coordinate1: Data[Source, Coordinate1]

    override def coordinates = IndexedSeq(coordinate1)
  }

  abstract class RichTuple2[-Source <: Shape.Any, +Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any]
      extends RichTuple[Source] with Tuple2[Source, Coordinate1, Coordinate2] {
    def coordinate1: Data[Source, Coordinate1]

    def coordinate2: Data[Source, Coordinate2]

    override def coordinates = IndexedSeq(coordinate1, coordinate2)
  }

  abstract class RichSequence[-Source <: Shape.Any, +Element <: Shape.Any]
      extends Sequence[Source, Element] with StateEquality[RichSequence[Source, Element]] {
    def elements: Seq[Data[Source, Element]]

    protected override def state = (sourceSchema, targetSchema, origin, elements)
  }

  abstract class RichOptional[-Source <: Shape.Any, +Value <: Shape.Concrete] extends Optional[Source, Value] {
    def hasValue: Boolean
  }

  abstract class RichSome[-Source <: Shape.Any, +Value <: Shape.Concrete] extends RichOptional[Source, Value] with Some[Source, Value] with StateEquality[RichSome[Source, Value]] {
    def value: Data[Source, Value]

    override def hasValue = true

    protected override def state = (sourceSchema, targetSchema, origin, value)
  }

  abstract class RichNone[-Source <: Shape.Any] extends RichOptional[Source, Nothing] with None[Source] with StateEquality[RichNone[Source]] {
    override def hasValue = false

    protected override def state = (sourceSchema, targetSchema, origin)
  }

  abstract class Factory {
    def primitive[Source <: Shape.Any, Raw: TypeTag](
      raw: Raw,
      origin: Origin[Source, Shape.Primitive[Raw]],
      schema: Schema.Primitive[Raw]): RichPrimitive[Source, Raw]

    def struct[Source <: Shape.Any](
      features: Map[String, Data[Shape, Shape.Any]],
      origin: Origin[Source, Shape.Struct],
      schema: Schema.Struct): RichStruct[Source]

    def tuple[Source <: Shape.Any](
      coordinates: IndexedSeq[Data[Source, Shape.Any]],
      origin: Origin[Source, Shape.Any],
      schema: Schema.Tuple): RichTuple[Source]

    def tuple[Source <: Shape.Any, Coordinate1 <: Shape.Any](
      coordinate1: Data[Source, Coordinate1],
      origin: Origin[Source, Shape.Tuple1[Coordinate1]],
      schema: Schema.Tuple1[Coordinate1]): RichTuple1[Source, Coordinate1]

    def tuple[Source <: Shape.Any, Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
      coordinate1: Data[Source, Coordinate1],
      coordinate2: Data[Source, Coordinate2],
      origin: Origin[Source, Shape.Tuple2[Coordinate1, Coordinate2]],
      schema: Schema.Tuple2[Coordinate1, Coordinate2]): RichTuple2[Source, Coordinate1, Coordinate2]

    def sequence[Source <: Shape.Any, Element <: Shape.Any](
      elements: Seq[Data[Source, Element]])(
        origin: Origin[Source, Shape.Sequence[Element]],
        schema: Schema.Sequence[Element]): RichSequence[Source, Element]

    def some[Source <: Shape.Any, Value <: Shape.Concrete](
      value: Data[Source, Value],
      origin: Origin[Source, Shape.Some[Value]],
      schema: Schema.Some[Value]): RichSome[Source, Value]

    def none[Source <: Shape.Any](origin: Origin[Source, Shape.None], schema: Schema.None): RichNone[Source]
  }
}

