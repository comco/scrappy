package com.github.comco.scrappy.pointer

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.Utils.RichBoolean
import com.github.comco.scrappy.picker.CoordinatePicker
import com.github.comco.scrappy.picker.ElementPicker
import com.github.comco.scrappy.picker.FeaturePicker
import com.github.comco.scrappy.picker.Picker
import com.github.comco.scrappy.picker.SelfPicker
import com.github.comco.scrappy.picker.SomePicker
import com.github.comco.scrappy.Shape

/**
 * Steps represent single-level traversion of data.
 * Steps are composed to form pointers.
 */
sealed abstract class Step[-SourceShape <: Shape.Any: TypeTag, +TargetShape <: Shape.Any: TypeTag] {
  def sourceType: Type.Any
  def targetType: Type[TargetShape]

  /**
   * The picker associated with this step.
   */
  def picker: Picker[SourceShape, TargetShape]

  def pointer: Pointer[SourceShape, TargetShape]
}

case class CoordinateStep(val sourceType: Type.Tuple, val position: Int)
    extends Step[Shape.Tuple, Shape.Any] {
  require(sourceType.hasCoordinate(position),
    s"Invalid position: $position for CoordinateType: $sourceType")

  def targetType = sourceType.coordinateType(position)

  def picker = CoordinatePicker(sourceType, position)
}

case class FeatureStep(val sourceType: StructType, val name: String)
    extends Step[Shape.Struct, Shape.Any] {
  require(sourceType.hasFeature(name),
    s"Invalid name: $name for StructType: $sourceType")

  def targetType = sourceType.featureType(name)

  def picker = FeaturePicker(sourceType, name)
}

case class ElementStep[ValueShape <: Shape.Any: TypeTag](val sourceType: Type.Seq[ValueShape], val index: Int)
    extends Step[Shape.Seq[ValueShape], ValueShape] {
  require(index >= 0, s"Invalid index: $index for creation of ElementStep")

  def targetType = sourceType.elementType

  def picker = ElementPicker(sourceType, index)
}

case class SomeStep[ValueShape <: Shape.Concrete: TypeTag](val sourceType: Type.Optional[ValueShape])
    extends Step[Shape.Optional[ValueShape], ValueShape] {
  def targetType = sourceType.valueType

  def picker = SomePicker(sourceType)
}