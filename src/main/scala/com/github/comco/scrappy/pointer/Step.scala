package com.github.comco.scrappy.pointer

import com.github.comco.scrappy.OptionType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.ElementPicker
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.Picker
import com.github.comco.scrappy.picker.SomePicker

/**
 * Steps represent single-level traversion of data.
 * Steps are composed to form pointers.
 */
sealed abstract class Step {
  def sourceType: Type
  def targetType: Type

  def picker: Picker
}

case class CoordinateStep(val sourceType: TupleType, val position: Int)
    extends Step {
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

case class SomeStep(val sourceType: OptionType)
    extends Step {
  def targetType = sourceType.someType

  def picker = SomePicker(sourceType)
}