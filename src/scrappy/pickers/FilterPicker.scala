package scrappy.pickers

import scrappy.Picker
import scrappy.BooleanType
import scrappy.SeqType
import scrappy.Value
import scrappy.SeqValue
import scrappy.BooleanValue

case class FilterPicker(booleanPicker: Picker) extends Picker {
  require(booleanPicker.resultType == BooleanType)
  
  val sourceType = SeqType(booleanPicker.sourceType)
  val resultType = sourceType
  
  protected def doPick(source: Value): SeqValue = (source: @unchecked) match {
    case SeqValue(_, elements) =>
      val newElements = elements.filter {
          booleanPicker.pick(_).asInstanceOf[BooleanValue].data
      }
      return SeqValue(resultType, newElements)
  }
}