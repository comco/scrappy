package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

case class AndThenPicker(val first: Picker, val next: Picker)
    extends Picker {
  require(first.targetType == next.sourceType)
  
  def sourceType = first.sourceType
  def targetType = next.targetType
  
  def pick(source: DataDomain.Data) = next.pick(first.pick(source))
  
  def pickWithOrigin(source: OriginatedDataDomain.Data) = 
    next.pickWithOrigin(first.pickWithOrigin(source))
}