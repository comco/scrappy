package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.data.SeqData

/**
 * Picker for an element of a seq.
 */
case class ElementPicker(val sourceType: SeqType, val index: Int)
    extends BaseSeqPicker {
  require(0 <= index)
  
  def targetType = sourceType.elementType
  
  def doPickData(source: SeqData) = source.elements(index)
  def doPickOriginatedData(source: OriginatedSeqData) = source.elements(index)
}