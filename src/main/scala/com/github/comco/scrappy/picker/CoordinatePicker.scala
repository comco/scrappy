package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy._
import com.github.comco.scrappy.Type._

/**
 * Picker for a coordinate of a tuple.
 */
case class CoordinateNPicker(val sourceType: Type.Tuple, val position: Int)
    extends Picker[Shape.Tuple, Shape.Any] {
  require(sourceType.hasCoordinate(position),
    s"TupleType: $sourceType does not have a coordinate at position: $position")

  def targetType = sourceType.coordinateType(position)

  def pickData(source: Data.Tuple) = source.coordinates(position)
  def pickOriginatedData(source: OriginatedData.Tuple) = source.coordinates(position)
}

case class Coordinate10Picker[+Coordinate1 <: Shape.Any: TypeTag](val sourceType: Type.Tuple1[Coordinate1]) extends Picker[Shape.Tuple1[Coordinate1], Coordinate1] {
  def targetType = sourceType.coordinate1Type

  def pickData(source: Data.Tuple1[Coordinate1]) = source.coordinate1
  def pickOriginatedData(source: OriginatedData.Tuple1[Coordinate1]) = source.coordinate1
}

case class Coordinate20Picker[+Coordinate1 <: Shape.Any: TypeTag, +Coordinate2 <: Shape.Any: TypeTag](val sourceType: Type.Tuple2[Coordinate1, Coordinate2])
    extends Picker[Shape.Tuple2[Coordinate1, Coordinate2], Coordinate1] {
  def targetType = sourceType.coordinate1Type

  def pickData(source: Data.Tuple2[Coordinate1, Coordinate2]) = source.coordinate1
  def pickOriginatedData(source: OriginatedData.Tuple2[Coordinate1, Coordinate2]) = source.coordinate1
}

case class Coordinate21Picker[+Coordinate1 <: Shape.Any: TypeTag, +Coordinate2 <: Shape.Any: TypeTag](val sourceType: Type.Tuple2[Coordinate1, Coordinate2])
    extends Picker[Shape.Tuple2[Coordinate1, Coordinate2], Coordinate2] {
  def targetType = sourceType.coordinate2Type

  def pickData(source: Data.Tuple2[Coordinate1, Coordinate2]) = source.coordinate2
  def pickOriginatedData(source: OriginatedData.Tuple2[Coordinate1, Coordinate2]) = source.coordinate2
}