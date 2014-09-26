package com.github.comco.scrappy.picker

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Composes pickers of comparable types together.
 */
case class AndThenPicker(val first: Picker, val next: Picker)
    extends Picker {
  require(first.targetType == next.sourceType)
  
  def sourceType = first.sourceType
  def targetType = next.targetType
  
  def pickData(source: Data) = next.pickData(first.pickData(source))
  
  def pickOriginatedData(source: OriginatedData) = 
    next.pickOriginatedData(first.pickOriginatedData(source))
}