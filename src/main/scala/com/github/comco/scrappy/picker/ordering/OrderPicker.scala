package com.github.comco.scrappy.picker.ordering

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.picker.BaseSeqPicker
import com.github.comco.scrappy.picker.Picker

case class OrderPicker(val by: Picker, val strategy: OrderingStrategy) extends BaseSeqPicker {
  require(by.targetType == strategy.datatype)

  def sourceType: SeqType = SeqType(by.sourceType)
  def targetType: SeqType = sourceType

  def doPickData(source: SeqData): SeqData = {
    val sortedElements = source.elements.sortBy(by.pickData(_))(strategy.dataOrdering)

    SeqData(targetType, sortedElements)
  }

  def doPickOriginatedData(source: OriginatedSeqData): OriginatedSeqData = {
    val sortedData = doPickData(source.data)
    val computedOrigin = source.origin.computed
    val sortedElements = source.elements.sortBy(by.pickOriginatedData(_))(strategy.originatedDataOrdering)

    OriginatedSeqData(sortedData, computedOrigin, sortedElements)
  }
}