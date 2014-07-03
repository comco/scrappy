package scrappy.pickers

import scrappy.Picker
import scrappy.SeqType
import scrappy.SeqValue
import scrappy.Value

case class IndexPicker(sourceType: SeqType, index: Int) extends Picker {
  require(0 <= index)
  
  val resultType = sourceType.elementType
  
  protected def doPick(source: Value): Value = (source: @unchecked) match {
    case SeqValue(_, elements) => elements(index)
  }
}