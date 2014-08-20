package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

case class AndThenPicker(val first: Picker, val next: Picker)
    extends Picker {
  require(first.targetType == next.sourceType)
  
  def sourceType = first.sourceType
  def targetType = next.targetType
  
  def pickData(source: DataDomain.Data) = next.pickData(first.pickData(source))
  
  def pickOriginatedData(source: OriginatedDataDomain.Data) = 
    next.pickOriginatedData(first.pickOriginatedData(source))
}