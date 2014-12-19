package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy._

/**
 * Picker for an element of a seq.
 */
case class ElementPicker[+Element <: Shape.Any: TypeTag](val sourceType: Type.Sequence[Element], val index: Int)
    extends Picker[Shape.Sequence[Element], Element] {
  require(0 <= index)

  def targetType = sourceType.elementType

  def pickData(source: Data.Sequence[Element]) = source.elements(index)
  def pickOriginatedData(source: OriginatedData.Sequence[Element]) = source.elements(index)
}