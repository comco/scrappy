package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.data.Data

/**
 * Picker for an element of a seq.
 */
case class ElementPicker(val sourceType: SeqType, val index: Int)
    extends Picker[SeqType, Type[Any]] {
  require(0 <= index)

  def targetType = sourceType.elementType

  def doPickData(source: Data[SeqType]) = source.elements(index)
  def doPickOriginatedData(source: OriginatedData[SeqType]) = source.elements(index)
}