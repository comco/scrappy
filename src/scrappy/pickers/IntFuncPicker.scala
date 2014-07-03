package scrappy.pickers

import scrappy.Picker
import scrappy.IntType
import scrappy.IntValue
import scrappy.Value

case class IntFunctionPicker(f: Int => Int) extends Picker {
  val sourceType = IntType
  val resultType = IntType
  
  protected def doPick(source: Value): IntValue = (source: @unchecked) match {
    case IntValue(v) => IntValue(f(v))
  }
}