package com.github.comco.scrappy.schema.basic

import scala.reflect.runtime.universe.TypeTag

import com.github.comco.scrappy.Schema
import com.github.comco.scrappy.Shape

object BasicFactory extends Schema.Factory {
  override def primitive[Raw: TypeTag] = BasicPrimitiveSchema[Raw]

  override def struct(name: String, featureSchemas: Map[String, Schema.Any]) = BasicStructSchema(name, featureSchemas)

  override def tuple(coordinateSchemas: IndexedSeq[Schema.Any]) = BasicTupleNSchema(coordinateSchemas)

  override def tuple[Coordinate1 <: Shape.Any](coordinate1Schema: Schema[Coordinate1]) = BasicTuple1Schema(coordinate1Schema)

  override def tuple[Coordinate1 <: Shape.Any, Coordinate2 <: Shape.Any](coordinate1Schema: Schema[Coordinate1], coordinate2Schema: Schema[Coordinate2]) =
    BasicTuple2Schema(coordinate1Schema, coordinate2Schema)

  override def sequence[Element <: Shape.Any](elementSchema: Schema[Element]) = BasicSequenceSchema(elementSchema)

  override def optional[Element <: Shape.Concrete](valueSchema: Schema[Element]) = BasicSomeSchema(valueSchema)

  override def none = BasicNoneSchema
}