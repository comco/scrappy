package scrappy.pickers

import scrappy.Picker
import scrappy.Type
import scrappy.IntType
import scrappy.IntValue
import scrappy.Value

case class IntConstPicker(val sourceType: Type, value: Int) extends Picker {
  
  val resultType = IntType
  
  protected def doPick(source: Value) = IntValue(value)
}