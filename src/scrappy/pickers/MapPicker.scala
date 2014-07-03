package scrappy.pickers

import scrappy.Picker
import scrappy.SeqType
import scrappy.SeqValue
import scrappy.Value

case class MapPicker(elementPicker: Picker) extends Picker {
  val sourceType = SeqType(elementPicker.sourceType)
  val resultType = SeqType(elementPicker.resultType)
  
  protected def doPick(source: Value): SeqValue = (source: @unchecked) match {
    case SeqValue(_, elements) => 
      SeqValue( resultType, elements.map(elementPicker.pick(_)))
  }
}