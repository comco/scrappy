package com.github.comco.scrappy.data.basic

import scala.reflect.runtime.universe.TypeTag
import com.github.comco.scrappy._
import com.github.comco.scrappy.data.FactoryCheckingMixin
import com.github.comco.scrappy.data.FactoryCheckingMixin

class BasicFactory extends Data.Factory {
  override def primitive[Raw: TypeTag](
    schema: Schema.Primitive[Raw],
    origin: Origin.Primitive[Raw],
    raw: Raw) =
    BasicPrimitiveData(schema, origin, raw)

  override def struct(
    schema: Schema.Struct,
    origin: Origin.Struct,
    features: Map[String, Data[Shape.Any]]) =
    BasicStructData(schema, origin, features)

  override def tuple(
    schema: Schema.Tuple,
    origin: Origin.Tuple,
    coordinates: IndexedSeq[Data[Shape.Any]]) =
    BasicTupleNData(schema, origin, coordinates)

  override def tuple[Coordinate1 <: Shape.Any](
    schema: Schema.Tuple1[Coordinate1],
    origin: Origin.Tuple1[Coordinate1],
    coordinate1: Data[Coordinate1]) =
    BasicTuple1Data(schema, origin, coordinate1)

  override def tuple[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](
    schema: Schema.Tuple2[Coordinate1, Coordinate2],
    origin: Origin.Tuple2[Coordinate1, Coordinate2],
    coordinate1: Data[Coordinate1],
    coordinate2: Data[Coordinate2]) =
    BasicTuple2Data(schema, origin, coordinate1, coordinate2)

  override def sequence[Element <: Shape.Any](
    schema: Schema.Sequence[Element],
    origin: Origin.Sequence[Element],
    elements: Seq[Data[Element]]) =
    BasicSequenceData(schema, origin, elements)

  override def some[Value <: Shape.Concrete](
    schema: Schema.Optional[Value],
    origin: Origin.Optional[Value],
    value: Data[Value]) =
    BasicSomeData(schema, origin, value)

  override def none(origin: Origin.None) = BasicNoneData(origin)
}

object BasicFactory extends BasicFactory