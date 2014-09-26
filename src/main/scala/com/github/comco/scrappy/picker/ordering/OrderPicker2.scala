package com.github.comco.scrappy.picker.ordering

import com.github.comco.scrappy.picker.Picker
import com.github.comco.scrappy.picker.BaseSeqPicker
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData

case class OrderPicker2(
    val by1: Picker, val strategy1: OrderingStrategy)(
    val by2: Picker, val strategy2: OrderingStrategy) extends BaseSeqPicker {
  require(by1.sourceType == by2.sourceType, "The two by-pickers should have the same sourceType.")
  require(by1.targetType == strategy1.datatype, "The first picker should have a targetType compatible with the first strategy.")
  require(by2.targetType == strategy2.datatype, "The second picker should have a targetType compatible with the second strategy.")
  
  def sourceType: SeqType = SeqType(by1.sourceType)
  def targetType: SeqType = SeqType(by1.sourceType)
  
  def dataOrdering = Ordering.Tuple2(strategy1.dataOrdering, strategy2.dataOrdering)
  def originatedDataOrdering = Ordering.Tuple2(strategy1.originatedDataOrdering, strategy2.originatedDataOrdering)
  
  def doPickData(source: SeqData): SeqData = {
    val sortedElements = source.elements.sortBy({
      element => (by1.pickData(element), by2.pickData(element))
    })(dataOrdering)
    
    SeqData(targetType, sortedElements)
  }
  
  def doPickOriginatedData(source: OriginatedSeqData): OriginatedSeqData = {
    val sortedData = doPickData(source.data)
    val computedOrigin = source.origin.computed
    val sortedElements = source.elements.sortBy({
      element => (by1.pickOriginatedData(element), by2.pickOriginatedData(element))
    })(originatedDataOrdering)
    
    OriginatedSeqData(sortedData, computedOrigin, sortedElements)
  }
}