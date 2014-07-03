package scrappy.pickers

import scrappy.Picker
import scrappy.Value

case class JoinPicker(parent: Picker, child: Picker) extends Picker {
  require(parent.resultType == child.sourceType)
  
  val sourceType = parent.sourceType
  val resultType = child.resultType
  
  protected def doPick(source: Value): Value = child.pick(parent.pick(source))
}