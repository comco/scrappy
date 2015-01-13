package com.github.comco.scrappy.data.basic

import scala.reflect.runtime.universe.TypeTag
import com.github.comco.scrappy._
import com.github.comco.scrappy.data.FactoryCheckingMixin

object BasicFactory extends Data.Factory with FactoryCheckingMixin {
  override def primitive[Source <: Shape.Any, Raw: TypeTag](
    raw: Raw,
    origin: Origin.Primitive[Raw],
    schema: Schema.Primitive[Raw]) =
    BasicPrimitiveData(raw, origin, schema)

  override def struct(
    features: Map[String, Data[Shape.Any]],
    origin: Origin.Struct,
    schema: Schema.Struct) =
    BasicStructData(features, origin, schema)

  override def tuple(
    coordinates: IndexedSeq[Data[Shape.Any]],
    origin: Origin.Tuple,
    schema: Schema.Tuple) =
    BasicTupleNData(coordinates, origin, schema)

  override def tuple[Coordinate1 <: Shape.Any](
    coordinate1: Data[Coordinate1],
    origin: Origin.Tuple1[Coordinate1],
    schema: Schema.Tuple1[Coordinate1]) =
    BasicTuple1Data(coordinate1, origin, schema)

  override def tuple[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
    coordinate1: Data[Coordinate1],
    coordinate2: Data[Coordinate2],
    origin: Origin.Tuple2[Coordinate1, Coordinate2],
    schema: Schema.Tuple2[Coordinate1, Coordinate2]) =
    BasicTuple2Data(coordinate1, coordinate2, origin, schema)

  override def sequence[Element <: Shape.Any](
    elements: Seq[Data[Element]],
    origin: Origin.Sequence[Element],
    schema: Schema.Sequence[Element]) =
    BasicSequenceData(elements, origin, schema)

  override def some[Value <: Shape.Concrete](
    value: Data[Value],
    origin: Origin.Some[Value],
    schema: Schema.Some[Value]) =
    BasicSomeData(value, origin, schema)

  override def none(
    origin: Origin.None,
    schema: Schema.None) =
    BasicNoneData(origin, schema)
}