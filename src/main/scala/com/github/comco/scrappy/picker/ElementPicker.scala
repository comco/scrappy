package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.Shape

/**
 * Picker for an element of a seq.
 */
case class ElementPicker[+ElementShape <: Shape.Any: TypeTag](val sourceType: Type.Seq[ElementShape], val index: Int)
    extends Picker[Shape.Seq[ElementShape], ElementShape] {
  require(0 <= index)

  def targetType = sourceType.elementType

  def doPickData(source: Data.Seq[ElementShape]) = source.elements(index)
  def doPickOriginatedData(source: OriginatedData.Seq[ElementShape]) = source.elements(index)
}