package scrappy.pickers

import scrappy.Picker
import scrappy.Type
import scrappy.Value

case class SelfPicker(val sourceType: Type) extends Picker {
  
  val resultType = sourceType
  
  protected def doPick(source: Value): Value = source
}