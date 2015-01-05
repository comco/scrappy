package com.github.comco.scrappy

import scala.reflect.runtime.universe._
import com.github.comco.scrappy.picker._

/**
 * Steps represent single-level traversion of data.
 * Steps are composed to form pointers.
 */
sealed abstract class Step[-Source <: Shape.Any, +Target <: Shape.Any] {
  def sourceType: Type[Source]
  def targetType: Type[Target]

  /**
   * The picker associated with this step.
   */
  def picker: Picker[Source, Target]
}

case class CoordinateStep(val sourceType: Type[Shape.Tuple], val position: Int)
    extends Step[Shape.Tuple, Shape.Any] {
  require(sourceType.hasCoordinate(position),
    s"Invalid position: $position for CoordinateType: $sourceType")

  def targetType = sourceType.coordinateType(position)

  def picker = CoordinateNPicker(sourceType, position)
}

case class FeatureStep(val sourceType: Type[Shape.Struct], val name: String)
    extends Step[Shape.Struct, Shape.Any] {
  require(sourceType.hasFeature(name),
    s"Invalid name: $name for StructType: $sourceType")

  def targetType = sourceType.featureType(name)

  def picker = FeaturePicker(sourceType, name)
}

case class ElementStep[+Element <: Shape.Any: TypeTag](val sourceType: Type[Shape.Sequence[Element]], val index: Int)
    extends Step[Shape.Sequence[Element], Element] {
  require(index >= 0, s"Invalid index: $index for creation of ElementStep")

  def targetType = sourceType.elementType

  def picker = ElementPicker(sourceType, index)
}

case class SomeStep[+Value <: Shape.Concrete: TypeTag](val sourceType: Type[Shape.Some[Value]])
    extends Step[Shape.Optional[Value], Value] {
  def targetType = sourceType.valueType

  def picker = SomePicker(sourceType)
}