package com.github.comco.scrappy

import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.ElementPicker
import com.github.comco.scrappy.picker.ValuePicker
import com.github.comco.scrappy.picker.Coordinate11Picker
import com.github.comco.scrappy.picker.Coordinate21Picker
import com.github.comco.scrappy.picker.Coordinate22Picker

sealed abstract class Step[-Source <: Shape.Any, +Target <: Shape.Any] {
  def sourceSchema: Schema.Any
  def targetSchema: Schema[Target]

  def picker: Picker[Source, Target]
}

case class FeatureStep(val sourceSchema: Schema.Struct, val featureName: String)
    extends Step[Shape.Struct, Shape.Any] {
  require(sourceSchema.featureSchemas.contains(featureName))

  override def targetSchema = sourceSchema.featureSchemas(featureName)

  override def picker = FeaturePicker(sourceSchema, featureName)
}

case class CoordinateStep(val sourceSchema: Schema.Tuple, val position: Int)
    extends Step[Shape.Tuple, Shape.Any] {
  require(sourceSchema.hasCoordinateAtPosition(position),
    s"Source schema: $sourceSchema should contain a coordinate at position: $position.")

  override def targetSchema = sourceSchema.coordinateSchemas(position)

  override def picker = CoordinatePicker(sourceSchema, position)
}

case class Coordinate11Step[Coordinate1 <: Shape.Any](
  val sourceSchema: Schema.Tuple1[Coordinate1])
    extends Step[Shape.Tuple1[Coordinate1], Coordinate1] {
  override def targetSchema = sourceSchema.coordinate1Schema

  override def picker = Coordinate11Picker(sourceSchema)
}

case class Coordinate21Step[Coordinate1 <: Shape.Any](
  val sourceSchema: Schema.Tuple2[Coordinate1, Nothing])
    extends Step[Shape.Tuple2[Coordinate1, Nothing], Coordinate1] {
  override def targetSchema = sourceSchema.coordinate1Schema

  override def picker = Coordinate21Picker(sourceSchema)
}

case class Coordinate22Step[Coordinate2 <: Shape.Any](
  val sourceSchema: Schema.Tuple2[Nothing, Coordinate2])
    extends Step[Shape.Tuple2[Nothing, Coordinate2], Coordinate2] {
  override def targetSchema = sourceSchema.coordinate2Schema

  override def picker = Coordinate22Picker(sourceSchema)
}

case class ElementStep[Element <: Shape.Any](val sourceSchema: Schema.Sequence[Element], val index: Int)
    extends Step[Shape.Sequence[Element], Element] {
  require(index >= 0, s"Index $index should be non-negative.")

  override def targetSchema = sourceSchema.elementSchema

  override def picker = ElementPicker(sourceSchema, index)
}

case class ValueStep[Value <: Shape.Concrete](val sourceSchema: Schema.Optional[Value])
    extends Step[Shape.Optional[Value], Value] {
  override def targetSchema = sourceSchema.valueSchema

  override def picker = ValuePicker(sourceSchema)
}