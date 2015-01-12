package com.github.comco.scrappy.data.basic

import scala.reflect.runtime.universe.TypeTag
import com.github.comco.scrappy._

object BasicFactory extends Data.Factory {
  override def primitive[Source <: Shape.Any, Raw: TypeTag](
    raw: Raw,
    origin: Origin[Source, Shape.Primitive[Raw]],
    schema: Schema.Primitive[Raw]) =
    BasicPrimitiveData(raw, origin, schema)

  override def struct[Source <: Shape.Any](
    features: Map[String, Data[Source, Shape.Any]],
    origin: Origin[Source, Shape.Struct],
    schema: Schema.Struct) =
    BasicStructData(features, origin, schema)

  override def tuple[Source <: Shape.Any](
    coordinates: IndexedSeq[Data[Source, Shape.Any]],
    origin: Origin[Source, Shape.Tuple],
    schema: Schema.Tuple) =
    BasicTupleNData(coordinates, origin, schema)

  override def tuple[Source <: Shape.Any, Coordinate1 <: Shape.Any](
    coordinate1: Data[Source, Coordinate1],
    origin: Origin[Source, Shape.Tuple1[Coordinate1]],
    schema: Schema.Tuple1[Coordinate1]) =
    BasicTuple1Data(coordinate1, origin, schema)

  override def tuple[Source <: Shape.Any, Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
    coordinate1: Data[Source, Coordinate1],
    coordinate2: Data[Source, Coordinate2])(
      origin: Origin[Source, Shape.Tuple2[Coordinate1, Coordinate2]],
      schema: Schema.Tuple2[Coordinate1, Coordinate2]) =
    BasicTuple2Data(coordinate1, coordinate2, origin, schema)

  override def sequence[Source <: Shape.Any, Element <: Shape.Any](
    elements: Seq[Data[Source, Element]],
    origin: Origin[Source, Shape.Sequence[Element]],
    schema: Schema.Sequence[Element]) =
    BasicSequenceData(elements, origin, schema)

  override def some[Source <: Shape.Any, Value <: Shape.Concrete](
    value: Data[Source, Value],
    origin: Origin[Source, Shape.Some[Value]],
    schema: Schema.Some[Value]) =
    BasicSomeData(value, origin, schema)

  override def none[Source <: Shape.Any](
    origin: Origin[Source, Shape.None],
    schema: Schema.None) =
    BasicNoneData(origin, schema)
}