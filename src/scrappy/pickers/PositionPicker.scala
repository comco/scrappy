package scrappy.pickers

import scrappy.Picker
import scrappy.TupleType
import scrappy.Value
import scrappy.TupleValue

case class PositionPicker(val sourceType: TupleType, val position: Int)
    extends Picker {
  require(0 <= position && position < sourceType.coordinateTypes.length)
  
  val resultType = sourceType.coordinateTypes(position)
  
  protected def doPick(source: Value): Value = (source: @unchecked) match {
    case TupleValue(_, coordinates) => coordinates(position)
  }
}