package com.github.comco.scrappy

import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.ElementPicker
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.SomePicker

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

  /**
   * Creates a single-step pointer from this step.
   */
  def mkPointer(): Pointer[Source, Target] = StepPointer(SelfPointer(sourceType), this)
}

case class CoordinateStep(val sourceType: Type[Shape.Tuple], val position: Int)
    extends Step[Shape.Tuple, Shape.Any] {
  require(sourceType.hasCoordinate(position),
    s"Invalid position: $position for CoordinateType: $sourceType")

  def targetType = sourceType.coordinateType(position)

  def picker = CoordinatePicker(sourceType, position)
}

case class FeatureStep(val sourceType: StructType, val name: String)
    extends Step {
  require(sourceType.hasFeature(name),
    s"Invalid name: $name for StructType: $sourceType")

  def targetType = sourceType.featureType(name)

  def picker = FeaturePicker(sourceType, name)
}

case class ElementStep(val sourceType: SeqType, val index: Int)
    extends Step {
  require(index >= 0, s"Invalid index: $index for creation of ElementStep")

  def targetType = sourceType.elementType

  def picker = ElementPicker(sourceType, index)
}

case class IntoStep(val sourceType: SeqType)
    extends Step {
  def targetType = sourceType.elementType

  def picker = SelfPicker(sourceType)
}

case class SomeStep(val sourceType: OptionType)
    extends Step {
  def targetType = sourceType.someType

  def picker = SomePicker(sourceType)
}